{ mkDerivation, base, bytestring, optparse-applicative, semigroups
, stdenv, system-filepath, text, time, transformers, void
}:
mkDerivation {
  pname = "optparse-generic";
  version = "1.2.0";
  sha256 = "1zhvrwcrchnls5fhy4m0466hhz84whrc2xm7nxwk1xjnmaqrb4l7";
  libraryHaskellDepends = [
    base bytestring optparse-applicative semigroups system-filepath
    text time transformers void
  ];
  description = "Auto-generate a command-line parser for your datatype";
  license = stdenv.lib.licenses.bsd3;
}
