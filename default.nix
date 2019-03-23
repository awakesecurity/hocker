{ mkDerivation, aeson, aeson-pretty, async, base, bytestring
, concurrentoutput, containers, cryptonite, data-fix, deepseq
, directory, exceptions, filepath, foldl, hnix, http-client
, http-types, lens, lens-aeson, lifted-base, megaparsec, memory
, mtl, neat-interpolation, network, network-uri, nix-paths
, optional-args, optparse-applicative, optparse-generic, pooled-io
, prettyprinter, pureMD5, scientific, stdenv, tar, tasty
, tasty-golden, tasty-hunit, tasty-quickcheck, tasty-smallcheck
, temporary, text, time, transformers, turtle, unordered-containers
, uri-bytestring, vector, word8, wreq, zlib
}:
mkDerivation {
  pname = "hocker";
  version = "1.0.5";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson aeson-pretty async base bytestring concurrentoutput
    containers cryptonite data-fix deepseq directory exceptions
    filepath foldl hnix http-client http-types lens lens-aeson
    lifted-base megaparsec memory mtl neat-interpolation network
    network-uri nix-paths optparse-applicative optparse-generic
    pooled-io prettyprinter pureMD5 scientific tar temporary text time
    transformers turtle unordered-containers uri-bytestring vector wreq
    zlib
  ];
  executableHaskellDepends = [
    base bytestring cryptonite data-fix filepath hnix lens mtl network
    optional-args optparse-applicative optparse-generic temporary text
  ];
  testHaskellDepends = [
    aeson base bytestring containers cryptonite mtl network network-uri
    prettyprinter tasty tasty-golden tasty-hunit tasty-quickcheck
    tasty-smallcheck text unordered-containers word8
  ];
  homepage = "https://github.com/awakesecurity/hocker#readme";
  description = "Interact with the docker registry and generate nix build instructions";
  license = stdenv.lib.licenses.asl20;
}
