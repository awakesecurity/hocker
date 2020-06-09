{ mkDerivation, base, base-compat, code-page, deepseq, directory
, filepath, ghc, ghc-paths, hspec, hspec-core, HUnit, mockery
, process, QuickCheck, setenv, silently, stdenv, stringbuilder, syb
, transformers
}:
mkDerivation {
  pname = "doctest";
  version = "0.17";
  sha256 = "ff12a52057335ff263db3e2ecd36cab2e0140ca5ae8c98bdfd4c7a83dfb31338";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base base-compat code-page deepseq directory filepath ghc ghc-paths
    process syb transformers
  ];
  executableHaskellDepends = [
    base base-compat code-page deepseq directory filepath ghc ghc-paths
    process syb transformers
  ];
  testHaskellDepends = [
    base base-compat code-page deepseq directory filepath ghc ghc-paths
    hspec hspec-core HUnit mockery process QuickCheck setenv silently
    stringbuilder syb transformers
  ];
  homepage = "https://github.com/sol/doctest#readme";
  description = "Test interactive Haskell examples";
  license = stdenv.lib.licenses.mit;
}
