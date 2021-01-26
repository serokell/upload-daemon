# SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: MPL-2.0

args:
let handleTest = path: (import path args).test;
in { upload-daemon = handleTest ./upload-daemon.nix; }
