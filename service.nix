# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: MPL-2.0

self:

{ pkgs, lib, config, ... }:
with lib;
let
  cfg = config.services.upload-daemon;
  description = "a daemon that asynchronously copies paths to a remote store";
  upload-paths = pkgs.writeShellScript "upload-paths" ''
    nix sign-paths -r -k ${cfg.post-build-hook.secretKey} $OUT_PATHS
    ${pkgs.netcat}/bin/nc -U ${cfg.socket} -N <<< $OUT_PATHS || echo "Uploading failed"
  '';
in
{
  options.services.upload-daemon = with types; {
    enable = mkEnableOption description;
    targets = mkOption {
      description = "List of stores to upload paths to";
      type = listOf str;
    };
    port = mkOption {
      description = "Port to listen for paths to upload";
      type = nullOr port;
      default = null;
    };
    socket = mkOption {
      description = "UNIX socket to listen on";
      type = nullOr path;
      default = "/run/upload-daemon/upload.sock";
    };
    prometheusPort = mkOption {
      description = "Port that prometheus endpoint listens on";
      type = nullOr port;
      default = 8082;
    };
    package = mkOption {
      description = "Package containing upload-daemon";
      type = package;
      default = self.defaultPackage.${pkgs.stdenv.system};
    };
    post-build-hook = {
      enable = mkEnableOption "post-build-hook that uploads the built path to a remote store";
      secretKey = mkOption {
        type = path;
        description = "Path to the key with which to sign the paths";
      };
    };
    workers = mkOption {
      description = "Number of nix-copies to run at the same time, null means use the number of CPUs";
      type = nullOr int;
      default = null;
      example = 4;
    };
    upload-derivations = mkEnableOption "uploading derivations for the built paths";
  };
  config = mkIf cfg.enable {
    systemd.services.upload-daemon = {
      inherit description;
      wantedBy = [ "multi-user.target" ];
      path = with pkgs; [ nix ];
      script =
        ''
          workers=${if cfg.workers == null then "$(nproc)" else toString cfg.workers}

          ${cfg.package}/bin/upload-daemon \
          ${lib.concatMapStringsSep " \\\n" (target: "--target '${target}'") cfg.targets} \
          ${lib.optionalString (! isNull cfg.port) "--port ${toString cfg.port}"} \
          ${lib.optionalString (! isNull cfg.socket) "--unix \"${toString cfg.socket}\""} \
          ${lib.optionalString (! isNull cfg.prometheusPort) "--stat-port ${toString cfg.prometheusPort}"} \
          ${lib.optionalString cfg.upload-derivations "--upload-derivations"} \
          -j "$workers" \
          +RTS -N"$workers"
      '';
        serviceConfig = {
          Restart = "always";
          RuntimeDirectory = "upload-daemon";
        };
    };
    nix.extraOptions = lib.optionalString cfg.post-build-hook.enable "post-build-hook = ${upload-paths}";
  };
}
