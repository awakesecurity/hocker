{ mkDerivation, base, bytestring, lib, Only, optparse-applicative
, system-filepath, text, time, transformers, void
}:
mkDerivation {
  pname = "optparse-generic";
  version = "1.4.4";
  sha256 = "e44853c0a3def2556cec31337db411d6404d7f81d505662f8ebac68e119bc077";
  revision = "1";
  editedCabalFile = "14vbfbl2va3s2pa4qjyyny1i15s2iw9993ld5b0qsqdv1z6nfjz1";
  libraryHaskellDepends = [
    base bytestring Only optparse-applicative system-filepath text time
    transformers void
  ];
  description = "Auto-generate a command-line parser for your datatype";
  license = lib.licenses.bsd3;
}
