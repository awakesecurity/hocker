{ mkDerivation, base, bytestring, fetchgit, optparse-applicative
, semigroups, stdenv, system-filepath, text, time, transformers
, void
}:
mkDerivation {
  pname = "optparse-generic";
  version = "1.2.0";
  src = fetchgit {
    url = "https://github.com/Gabriel439/Haskell-Optparse-Generic-Library";
    sha256 = "0dy7z16287pf26bjxams8xz4i6is34l2a5qzpsqgap93i3s8p256";
    rev = "f8449f420b5595001128a80282538bdbd75bf030";
  };
  libraryHaskellDepends = [
    base bytestring optparse-applicative semigroups system-filepath
    text time transformers void
  ];
  description = "Auto-generate a command-line parser for your datatype";
  license = stdenv.lib.licenses.bsd3;
}
