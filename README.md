# upload-daemon

A daemon that asynchronously copies paths to a remote store.

## Building

Build with `nix-build` or `nix build`

## Usage

You must provide `-t/--target` argument, which should be a valid nix store url, and at least one of `-p/--port` or `-u/--unix` for a control socket.

There's a NixOS module in [./service.nix](./service.nix) available as `nixosModules.upload-daemon` which adds a systemd service that runs upload-daemon for you and optionally configures a nix post-build-hook that signs and uploads all paths that were built on the machine to the remote store.
