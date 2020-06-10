{ mkDerivation, base, base16-bytestring, bytestring, criterion, SHA
, stdenv, tasty, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "cryptohash-sha256";
  version = "0.11.101.0";
  sha256 = "52756435dbea248e344fbcbcc5df5307f60dfacf337dfd11ae30f1c7a4da05dd";
  revision = "4";
  editedCabalFile = "00p6sx2n1pzykm3my68hjfk8l66g66rh7inrfcnkd5mhilqdcqxr";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base bytestring ];
  testHaskellDepends = [
    base base16-bytestring bytestring SHA tasty tasty-hunit
    tasty-quickcheck
  ];
  benchmarkHaskellDepends = [ base bytestring criterion ];
  homepage = "https://github.com/hvr/cryptohash-sha256";
  description = "Fast, pure and practical SHA-256 implementation";
  license = stdenv.lib.licenses.bsd3;
}
