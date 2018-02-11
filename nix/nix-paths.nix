{ mkDerivation, base, nix, process, stdenv }:
mkDerivation {
  pname = "nix-paths";
  version = "1.0.1";
  sha256 = "1y09wl1ihxmc9p926g595f70pdcsx78r3q5n5rna23lpq8xicdxb";
  libraryHaskellDepends = [ base process ];
  libraryToolDepends = [ nix ];
  homepage = "https://github.com/peti/nix-paths";
  description = "Knowledge of Nix's installation directories";
  license = stdenv.lib.licenses.bsd3;
}
