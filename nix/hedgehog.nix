{ mkDerivation, ansi-terminal, async, base, bytestring
, concurrent-output, containers, directory, erf, exceptions, fail
, lifted-async, mmorph, monad-control, mtl, pretty-show, primitive
, random, resourcet, semigroups, stdenv, stm, template-haskell
, text, time, transformers, transformers-base, wl-pprint-annotated
}:
mkDerivation {
  pname = "hedgehog";
  version = "1.0.2";
  sha256 = "8e0cef556c2a9386d45b8d29e8051de99693d19b1d29323add108ecd5c69a880";
  revision = "3";
  editedCabalFile = "0y9glrf68jx8h8wsrrcdgxwmf0im1rh3g050mf0lk8iv0cdvdd2m";
  libraryHaskellDepends = [
    ansi-terminal async base bytestring concurrent-output containers
    directory erf exceptions fail lifted-async mmorph monad-control mtl
    pretty-show primitive random resourcet semigroups stm
    template-haskell text time transformers transformers-base
    wl-pprint-annotated
  ];
  testHaskellDepends = [
    base containers mmorph mtl pretty-show semigroups text transformers
  ];
  homepage = "https://hedgehog.qa";
  description = "Release with confidence";
  license = stdenv.lib.licenses.bsd3;
}
