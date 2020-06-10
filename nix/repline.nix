{ mkDerivation, base, containers, exceptions, haskeline, mtl
, process, stdenv
}:
mkDerivation {
  pname = "repline";
  version = "0.3.0.0";
  sha256 = "e82114f9af6b8967c18d712415adf71e9759324b849fd7948e58bcf7bc83315a";
  libraryHaskellDepends = [
    base containers exceptions haskeline mtl process
  ];
  testHaskellDepends = [ base containers mtl process ];
  homepage = "https://github.com/sdiehl/repline";
  description = "Haskeline wrapper for GHCi-like REPL interfaces";
  license = stdenv.lib.licenses.mit;
}
