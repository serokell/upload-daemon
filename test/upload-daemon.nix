# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: MPL-2.0

{ self, pkgs, nixosPath, ... }@args:

(import "${nixosPath}/tests/make-test-python.nix" ({ pkgs, ... }: {
  name = "upload-daemon";

  nodes = {

    runner = { pkgs, ... }: {
      imports = [ self.nixosModules.upload-daemon ];

      environment.systemPackages = [ pkgs.stdenv ];

      nix.binaryCachePublicKeys = [ (builtins.readFile ./cache-pub-key.pem) ];

      services.upload-daemon = {
        enable = true;
        post-build-hook = {
          enable = true;
          secretKey = ./cache-priv-key.pem;
        };
        targets = [ "/tmp/test1" "/tmp/test2" ];
      };
    };

  };

  testScript =''
    start_all()

    runner.wait_for_unit("multi-user.target")
    runner.wait_for_unit("upload-daemon.service")

    runner.succeed(
        "nix-build --no-substitute ${./test-build.nix}"
    )
    runner.succeed("sleep 1")
    runner.succeed(
        "cat /tmp/test1${pkgs.callPackage ./test-build.nix {}} | grep hello"
    )
    runner.succeed(
        "cat /tmp/test2${pkgs.callPackage ./test-build.nix {}} | grep hello"
    )
  '';
})) args
