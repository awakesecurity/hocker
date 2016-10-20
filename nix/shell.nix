{ ghc }:
let
  config = import ../config.nix;
  pkgs = import <nixpkgs> { inherit config; };
in with pkgs; pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "hocker-stack-shell";
  buildInputs = [
    zlib cabal-install
  ];
}
