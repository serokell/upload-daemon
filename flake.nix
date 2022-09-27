# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: MPL-2.0

{
  description = "";

  inputs.nixpkgs.url = "github:serokell/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" ] (system: {
      packages = {
        upload-daemon =
          nixpkgs.legacyPackages.${system}.haskellPackages.callCabal2nix
          "upload-daemon" ./. { };
      };

      defaultPackage = self.packages.${system}.upload-daemon;

      defaultApp = {
        type = "app";
        program = "${self.defaultPackage.${system}}/bin/upload-daemon";
      };

      checks = {
        build = self.defaultPackage.${system};
      } // (import ./test {
        inherit self system;
        pkgs = nixpkgs.legacyPackages.${system};
        nixosPath = "${nixpkgs}/nixos";
      });

      devShell = with nixpkgs.legacyPackages.${system}; mkShell {
        inputsFrom = [ self.packages.${system}.upload-daemon.env ];
        buildInputs = [ haskell-language-server ];
      };
    }) // {
      nixosModules.upload-daemon = import ./service.nix self;
    };
}
