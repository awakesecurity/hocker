{ mkDerivation, base, bytestring, optparse-applicative, semigroups
, stdenv, system-filepath, text, time, transformers, void
}:
mkDerivation {
  pname = "optparse-generic";
  version = "1.1.5";
  sha256 = "1xg6c7h6h8q64gwskh7l4h7qn7w4y0ixf88grgk23xdficgmsyms";
  libraryHaskellDepends = [
    base bytestring optparse-applicative semigroups system-filepath
    text time transformers void
  ];
  description = "Auto-generate a command-line parser for your datatype";
  license = stdenv.lib.licenses.bsd3;
}
