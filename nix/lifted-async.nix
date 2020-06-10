{ mkDerivation, async, base, constraints, criterion, deepseq, HUnit
, lifted-base, monad-control, mtl, stdenv, tasty
, tasty-expected-failure, tasty-hunit, tasty-th, transformers-base
}:
mkDerivation {
  pname = "lifted-async";
  version = "0.10.0.6";
  sha256 = "72f5563fc622fe0e218e393287c49a63eec7207330cd9fdbe73416db745ad0da";
  libraryHaskellDepends = [
    async base constraints lifted-base monad-control transformers-base
  ];
  testHaskellDepends = [
    async base HUnit lifted-base monad-control mtl tasty
    tasty-expected-failure tasty-hunit tasty-th
  ];
  benchmarkHaskellDepends = [ async base criterion deepseq ];
  homepage = "https://github.com/maoe/lifted-async";
  description = "Run lifted IO operations asynchronously and wait for their results";
  license = stdenv.lib.licenses.bsd3;
}
