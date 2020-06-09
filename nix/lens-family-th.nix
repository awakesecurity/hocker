{ mkDerivation, base, hspec, stdenv, template-haskell, transformers
}:
mkDerivation {
  pname = "lens-family-th";
  version = "0.5.1.0";
  sha256 = "119392e22060bc4e8ce5ecec82f6162eef0b53312f0363c826b009855e74f2be";
  libraryHaskellDepends = [ base template-haskell ];
  testHaskellDepends = [ base hspec template-haskell transformers ];
  homepage = "http://github.com/DanBurton/lens-family-th#readme";
  description = "Generate lens-family style lenses";
  license = stdenv.lib.licenses.bsd3;
}
