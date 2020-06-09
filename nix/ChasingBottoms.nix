{ mkDerivation, array, base, containers, mtl, QuickCheck, random
, stdenv, syb
}:
mkDerivation {
  pname = "ChasingBottoms";
  version = "1.3.1.8";
  sha256 = "9d873281fe529f21502714ee6f44df73c9a268e8557c80c9815c95d8215c4dd5";
  libraryHaskellDepends = [
    base containers mtl QuickCheck random syb
  ];
  testHaskellDepends = [
    array base containers mtl QuickCheck random syb
  ];
  description = "For testing partial and infinite values";
  license = stdenv.lib.licenses.mit;
}
