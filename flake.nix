# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: MPL-2.0

{
  description = "";

  inputs.nixpkgs.url = "github:serokell/nixpkgs";

  outputs = { self, nixpkgs }: {
    packages = builtins.mapAttrs (system: pkgs: {
      upload-daemon =
        pkgs.haskellPackages.callCabal2nix "upload-daemon" ./. { };
    }) nixpkgs.legacyPackages;

    defaultPackage =
      builtins.mapAttrs (_: packages: packages.upload-daemon) self.packages;

    defaultApp = builtins.mapAttrs (_: pkg: {
      type = "app";
      program = "${pkg}/bin/upload-daemon";
    }) self.defaultPackage;

    nixosModules.upload-daemon = import ./service.nix;
  };
}
