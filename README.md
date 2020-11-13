# upload-daemon

A daemon that asynchronously copies paths to a remote store.

## Building

Build with `nix-build` or `nix build`

## Usage

You must provide `-t/--target` argument, which should be a valid nix store url, and at least one of `-p/--port` or `-u/--unix` for a control socket.

There's a NixOS module in [./service.nix](./service.nix) available as `nixosModules.upload-daemon` which adds a systemd service that runs upload-daemon for you and optionally configures a nix post-build-hook that signs and uploads all paths that were built on the machine to the remote store.

## License

Licensed under Mozilla Public License version 2. Read [./LICENSE](./LICENSE) for more details.

## About Serokell

Xrefcheck is maintained and funded with ❤ by [Serokell](https://serokell.io/).
The names and logo for Serokell are trademark of Serokell OÜ.

We love open source software! See [our other projects](https://serokell.io/community?utm_source=github) or [hire us](https://serokell.io/hire-us?utm_source=github) to design, develop and grow your idea!
