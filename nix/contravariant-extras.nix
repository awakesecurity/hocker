{ mkDerivation, base, contravariant, stdenv, template-haskell
, template-haskell-compat-v0208
}:
mkDerivation {
  pname = "contravariant-extras";
  version = "0.3.5.1";
  sha256 = "e9037c990ec8c5d92f75d99e0dde0667dac59f1235dd6be3d990979aab792b65";
  libraryHaskellDepends = [
    base contravariant template-haskell template-haskell-compat-v0208
  ];
  homepage = "https://github.com/nikita-volkov/contravariant-extras";
  description = "Extras for the \"contravariant\" package";
  license = stdenv.lib.licenses.mit;
}
