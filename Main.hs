-- SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
--
-- SPDX-License-Identifier: MPL-2.0

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Main (main) where

import Conduit
import Control.Concurrent
import Control.Concurrent.Async (async, Concurrently(Concurrently), runConcurrently)
import Control.Monad (forM_, forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit.List as CL
import Data.Conduit.Network as TCP
import Data.Conduit.Network.Unix as UNIX
import Data.Semigroup ((<>))
import Data.Streaming.Network (HasReadWrite)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Options.Applicative
  (Parser, auto, execParser, fullDesc, header, help, helper, info, long, metavar, option, optional,
  progDesc, short, showDefault, strOption, value, (<**>))
import System.Exit (ExitCode(ExitSuccess))
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Metrics.Prometheus.Concurrent.RegistryT (registerCounter, registerGauge, runRegistryT)
import System.Metrics.Prometheus.Http.Scrape (serveMetricsT)
import System.Metrics.Prometheus.Metric.Counter as C (Counter, inc)
import System.Metrics.Prometheus.Metric.Gauge as G (Gauge, dec, inc)
import System.Metrics.Prometheus.MetricId (addLabel, fromList)
import System.Process (readProcessWithExitCode)

data StatisticsHandlers
  = StatisticsHandlers
  { requests :: Counter
  , success  :: Gauge
  , failure  :: Gauge
  , running  :: Gauge
  , queued   :: Gauge
  }

type UploadTarget = String

data UploadOptions = UploadOptions
  { port :: Maybe Int
  , unix :: Maybe FilePath
  , prometheusPort :: Int
  , uploadTarget :: UploadTarget
  , nrWorkers :: Int
  }

uploadOptions :: Parser UploadOptions
uploadOptions = UploadOptions
  <$> optional
  ( option auto
    ( long "port"
    <> short 'p'
    <> metavar "PORT"
    <> help "TCP port to listen on" ) )
  <*> optional
  ( strOption
  ( long "unix"
    <> short 'u'
    <> metavar "UNIX"
    <> help "UNIX Domain Socket to listen on" ) )
  <*> option auto
  ( long "stat-port"
    <> short 's'
    <> metavar "SPORT"
    <> value 8081
    <> showDefault
    <> help "Prometheus listening port" )
  <*> strOption
  ( long "target"
    <> short 't'
    <> metavar "TARGET"
    <> help "Where to upload" )
  <*> option auto
  ( long "workers"
    <> short 'j'
    <> metavar "WORKERS"
    <> value 2
    <> showDefault
    <> help "Number of nix-copies to run at the same time" )


-- | Upload a path to target binary cache
upload :: UploadTarget -> StatisticsHandlers -> ByteString -> IO ()
upload target StatisticsHandlers {..} path = do
  G.dec queued
  G.inc running
  (code, _, stderrOutput) <- readProcessWithExitCode "nix"
    [ "copy", "--to", target, unpack $ decodeUtf8 path ]
    ""
  G.dec running
  if code /= ExitSuccess
    then G.inc failure >> hPutStrLn stderr stderrOutput
    else G.inc success >> hPutStr stderr "Uploaded " >> BS.hPutStrLn stderr path


uploadWorker :: UploadTarget -> StatisticsHandlers -> Chan ByteString -> IO ()
uploadWorker target shand uploadCh = forever $
  readChan uploadCh >>= upload target shand

response :: [ByteString] -> BS.ByteString
response paths = BS.pack $ "Queued " <> show (length paths) <> " paths"

logUploading :: ConduitT BS.ByteString Void IO ()
logUploading = CL.mapM_ $ \paths -> BS.hPutStrLn stderr $ "Queued " <> paths

queueUpload :: Chan ByteString -> StatisticsHandlers -> ConduitT BS.ByteString BS.ByteString IO ()
queueUpload uploadCh StatisticsHandlers {..} =
    passthroughSink logUploading return
    .| passthroughSink (CL.mapM_ $ const $ C.inc requests) return
    .| CL.map BS.words
    .| passthroughSink (CL.mapM_ . mapM_ $ const $ G.inc queued) return
    .| passthroughSink (CL.mapM_ . mapM_ $ writeChan uploadCh) return
    .| CL.map response

handleConnection :: (HasReadWrite ad) => ConduitT BS.ByteString BS.ByteString IO () -> ad -> IO ()
handleConnection conduit appData = runConduit $ appSource appData .| conduit .| appSink appData

main :: IO ()
main = do
  UploadOptions{..} <- execParser opts


  when ((port, unix) == (Nothing, Nothing)) $ error "Specify either --port or --unix to start the server"

  hPutStrLn stderr $ "Starting server on "
    <> maybe "" (\p -> "localhost:" <> show p <> " ") port
    <> maybe "" (show <> const " ") unix
    <> "uploading to " <> uploadTarget
  uploadCh <- newChan
  runRegistryT $ do
    shand <- StatisticsHandlers
      <$> registerCounter "requests_total" mempty
      <*> registerGauge "uploads" (fromList [("upload", "success")])
      <*> registerGauge "uploads" (addLabel "upload" "failure" mempty)
      <*> registerGauge "uploads" (addLabel "upload" "running" mempty)
      <*> registerGauge "uploads" (addLabel "upload" "queued" mempty)
    let conduit = queueUpload uploadCh shand

    void $ liftIO $ async $ runConcurrently
      $ Concurrently (forM_ port $ \p -> runTCPServer (TCP.serverSettings p "*") $ handleConnection conduit)
      <>Concurrently (forM_ unix $ \u -> runUnixServer (UNIX.serverSettings u) $ handleConnection conduit)
      <>(mconcat . replicate nrWorkers $ Concurrently $ uploadWorker uploadTarget shand uploadCh)

    serveMetricsT prometheusPort [ "metrics" ]
  where
    opts = info (uploadOptions <**> helper)
      ( fullDesc
     <> progDesc "Listen on PORT and/or UNIX for paths to be uploaded to TARGET"
     <> header "nix-upload-daemon - keep a queue of uploading paths to a remote store" )
