{ mkDerivation, array, async, base, base64-bytestring
, blaze-builder, bytestring, case-insensitive, containers, cookie
, deepseq, directory, exceptions, filepath, ghc-prim, hspec
, http-types, mime-types, monad-control, network, network-uri
, random, stdenv, streaming-commons, text, time, transformers, zlib
}:
mkDerivation {
  pname = "http-client";
  version = "0.5.6.1";
  sha256 = "1v9bdb8dkhb5g6jl9azk86ig7ia8xh9arr64n7s8r94fp0vl6c1c";
  libraryHaskellDepends = [
    array base base64-bytestring blaze-builder bytestring
    case-insensitive containers cookie deepseq exceptions filepath
    ghc-prim http-types mime-types network network-uri random
    streaming-commons text time transformers
  ];
  testHaskellDepends = [
    async base base64-bytestring blaze-builder bytestring
    case-insensitive containers deepseq directory hspec http-types
    monad-control network network-uri streaming-commons text time
    transformers zlib
  ];
  doCheck = false;
  homepage = "https://github.com/snoyberg/http-client";
  description = "An HTTP client engine";
  license = stdenv.lib.licenses.mit;
}
