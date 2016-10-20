{ mkDerivation, base, bytestring, case-insensitive, connection
, cryptonite, data-default-class, exceptions, hspec, http-client
, http-types, memory, network, stdenv, tls, transformers
}:
mkDerivation {
  pname = "http-client-tls";
  version = "0.3.3";
  sha256 = "0r50h7lhrwmxcmiq5nw1rxnpda3k6mhz4jsd86m56ymai5lnf77c";
  libraryHaskellDepends = [
    base bytestring case-insensitive connection cryptonite
    data-default-class exceptions http-client http-types memory network
    tls transformers
  ];
  testHaskellDepends = [ base hspec http-client http-types ];
  doCheck = false;
  homepage = "https://github.com/snoyberg/http-client";
  description = "http-client backend using the connection package and tls library";
  license = stdenv.lib.licenses.mit;
}
