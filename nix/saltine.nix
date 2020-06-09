{ mkDerivation, base, bytestring, hashable, libsodium, profunctors
, QuickCheck, semigroups, stdenv, test-framework
, test-framework-quickcheck2
}:
mkDerivation {
  pname = "saltine";
  version = "0.1.1.0";
  sha256 = "8e97995d4b7f9ceb1f22865e217cde5da9c15ac69c6992467a58e59a06f3ecaa";
  libraryHaskellDepends = [ base bytestring hashable profunctors ];
  libraryPkgconfigDepends = [ libsodium ];
  testHaskellDepends = [
    base bytestring QuickCheck semigroups test-framework
    test-framework-quickcheck2
  ];
  description = "Cryptography that's easy to digest (NaCl/libsodium bindings)";
  license = stdenv.lib.licenses.mit;
}
