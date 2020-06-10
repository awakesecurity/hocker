{ mkDerivation, base, hedgehog, stdenv, tagged, tasty
, tasty-expected-failure
}:
mkDerivation {
  pname = "tasty-hedgehog";
  version = "1.0.0.2";
  sha256 = "65538f7371f5ccf52c232a4723d931dd0278ea49bf478c2abe50c3bc4c1d5bef";
  revision = "2";
  editedCabalFile = "1v4jp3xk5ikik638vkyf2jxkhaf2n6fsw8zxqxxjv65x60082kl7";
  libraryHaskellDepends = [ base hedgehog tagged tasty ];
  testHaskellDepends = [
    base hedgehog tasty tasty-expected-failure
  ];
  homepage = "https://github.com/qfpl/tasty-hedgehog";
  description = "Integration for tasty and hedgehog";
  license = stdenv.lib.licenses.bsd3;
}
