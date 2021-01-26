# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: MPL-2.0

{ system ? builtins.currentSystem }: derivation {
  name = "test-build";
  inherit system;
  builder = "/bin/sh";
  args = [(builtins.toFile "builder.sh" "echo hello > $out")];
}
