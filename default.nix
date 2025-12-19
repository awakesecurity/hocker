{ compiler ? "ghc910" }:
let
  pkgs = import ./nix/pkgs.nix { inherit compiler; };
in
{
  inherit (pkgs.haskellPackages) hocker;
}
