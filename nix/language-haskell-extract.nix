{ mkDerivation, base, regex-posix, stdenv, template-haskell }:
mkDerivation {
  pname = "language-haskell-extract";
  version = "0.2.4";
  sha256 = "14da16e56665bf971723e0c5fd06dbb7cc30b4918cf8fb5748570785ded1acdb";
  revision = "1";
  editedCabalFile = "1chx4g8ngb1hpyh3r9rbl8rkjkm67klms4wmw3p1g2llg47vvqip";
  libraryHaskellDepends = [ base regex-posix template-haskell ];
  homepage = "http://github.com/finnsson/template-helper";
  description = "Module to automatically extract functions from the local code";
  license = stdenv.lib.licenses.bsd3;
}
