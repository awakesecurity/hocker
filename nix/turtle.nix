{ mkDerivation, ansi-wl-pprint, async, base, bytestring, clock
, containers, criterion, directory, doctest, exceptions, fail
, foldl, hostname, managed, optional-args, optparse-applicative
, process, semigroups, stdenv, stm, streaming-commons
, system-fileio, system-filepath, temporary, text, time
, transformers, unix, unix-compat
}:
mkDerivation {
  pname = "turtle";
  version = "1.5.19";
  sha256 = "2f04012e363f8be263274a13b261df5a42451e29b0b83526cac599183eac1d1a";
  revision = "1";
  editedCabalFile = "1z0wjrd25k7zc0bvsy1cxicfml0sdchs7sfr6fz5jlnlggpbn0fq";
  libraryHaskellDepends = [
    ansi-wl-pprint async base bytestring clock containers directory
    exceptions foldl hostname managed optional-args
    optparse-applicative process semigroups stm streaming-commons
    system-fileio system-filepath temporary text time transformers unix
    unix-compat
  ];
  testHaskellDepends = [
    base doctest fail system-filepath temporary
  ];
  benchmarkHaskellDepends = [ base criterion text ];
  description = "Shell programming, Haskell-style";
  license = stdenv.lib.licenses.bsd3;
}
