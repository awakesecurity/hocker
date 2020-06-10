{ mkDerivation, base, base16-bytestring, bytestring, criterion, SHA
, stdenv, tasty, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "cryptohash-sha512";
  version = "0.11.100.1";
  sha256 = "10698bb9575eaa414a65d9644caa9408f9276c63447406e0a4faef91db1071a9";
  revision = "4";
  editedCabalFile = "0iqs51a58w71j1zz3rn9kical63yvvqqqrc6971mh6wfscyi1gqr";
  libraryHaskellDepends = [ base bytestring ];
  testHaskellDepends = [
    base base16-bytestring bytestring SHA tasty tasty-hunit
    tasty-quickcheck
  ];
  benchmarkHaskellDepends = [ base bytestring criterion ];
  homepage = "https://github.com/hvr/cryptohash-sha512";
  description = "Fast, pure and practical SHA-512 implementation";
  license = stdenv.lib.licenses.bsd3;
}
