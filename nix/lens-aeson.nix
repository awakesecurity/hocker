{ mkDerivation, aeson, attoparsec, base, bytestring, Cabal
, cabal-doctest, doctest, generic-deriving, lens, scientific
, semigroups, simple-reflect, stdenv, text, unordered-containers
, vector
}:
mkDerivation {
  pname = "lens-aeson";
  version = "1.1";
  sha256 = "f7bc9c6f95735b523afac6316195d06b31f9b85c84918960096e4eecdb6cc90e";
  revision = "2";
  editedCabalFile = "1ivxsj7igrrrzkwhw7ipcxnnr721797is6yfsrh3mha9dl8985sf";
  setupHaskellDepends = [ base Cabal cabal-doctest ];
  libraryHaskellDepends = [
    aeson attoparsec base bytestring lens scientific text
    unordered-containers vector
  ];
  testHaskellDepends = [
    base doctest generic-deriving semigroups simple-reflect
  ];
  homepage = "http://github.com/lens/lens-aeson/";
  description = "Law-abiding lenses for aeson";
  license = stdenv.lib.licenses.mit;
}
