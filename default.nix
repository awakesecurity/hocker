{ mkDerivation, aeson, aeson-pretty, ansi-wl-pprint, async, base
, bytestring, concurrentoutput, containers, cryptonite, data-fix
, deepseq, directory, exceptions, filepath, foldl, hnix
, http-client, http-types, lens, lens-aeson, lifted-base, memory
, mtl, neat-interpolation, network, network-uri, optional-args
, optparse-applicative, optparse-generic, pooled-io, pureMD5
, scientific, stdenv, tar, tasty, tasty-golden, tasty-hunit
, tasty-quickcheck, tasty-smallcheck, temporary, text, time
, transformers, turtle, unordered-containers, uri-bytestring
, vector, wreq, zlib
}:
mkDerivation {
  pname = "hocker";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty ansi-wl-pprint async base bytestring
    concurrentoutput containers cryptonite data-fix deepseq directory
    exceptions filepath foldl hnix http-client http-types lens
    lens-aeson lifted-base memory mtl neat-interpolation network
    network-uri optional-args optparse-applicative optparse-generic
    pooled-io pureMD5 scientific tar temporary text time transformers
    turtle unordered-containers uri-bytestring vector wreq zlib
  ];
  executableHaskellDepends = [
    base bytestring cryptonite data-fix filepath hnix lens mtl network
    optional-args optparse-applicative optparse-generic temporary text
  ];
  testHaskellDepends = [
    aeson ansi-wl-pprint base bytestring containers cryptonite mtl
    network network-uri tasty tasty-golden tasty-hunit tasty-quickcheck
    tasty-smallcheck text unordered-containers
  ];
  homepage = "https://github.com/awakenetworks/hocker#readme";
  description = "CLI tools and library to interact with a V2 Docker Registry";
  license = stdenv.lib.licenses.asl20;
}
