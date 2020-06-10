{ mkDerivation, aeson, base, bytestring, containers, deepseq
, directory, filepath, hspec, parsec, stdenv, text, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "microstache";
  version = "1.0.1.1";
  sha256 = "5de98542313eb75f84961366ff8a70ed632387ba6518215035b2dd1b32d6a120";
  revision = "7";
  editedCabalFile = "05ia18kywpmk01sqnywflfq0ck3yivh8rc178f575py1zrdpn3l7";
  libraryHaskellDepends = [
    aeson base bytestring containers deepseq directory filepath parsec
    text transformers unordered-containers vector
  ];
  testHaskellDepends = [
    aeson base bytestring containers hspec parsec text
  ];
  homepage = "https://github.com/phadej/microstache";
  description = "Mustache templates for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
