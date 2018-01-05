{ mkDerivation, ansi-wl-pprint, async, base, bytestring, clock
, containers, criterion, directory, doctest, foldl, hostname
, managed, optional-args, optparse-applicative, process, semigroups
, stdenv, stm, system-fileio, system-filepath, temporary, text
, time, transformers, unix, unix-compat
}:
mkDerivation {
  pname = "turtle";
  version = "1.4.5";
  sha256 = "082svk0bcf1vvqrzfmb6r5rridgch0c15423fwcb57cfc8xzm8kx";
  libraryHaskellDepends = [
    ansi-wl-pprint async base bytestring clock containers directory
    foldl hostname managed optional-args optparse-applicative process
    semigroups stm system-fileio system-filepath temporary text time
    transformers unix unix-compat
  ];
  testHaskellDepends = [ base doctest system-filepath temporary ];
  benchmarkHaskellDepends = [ base criterion text ];
  description = "Shell programming, Haskell-style";
  license = stdenv.lib.licenses.bsd3;
}
