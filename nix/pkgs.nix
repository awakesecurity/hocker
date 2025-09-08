let
  # "nixos-unstable" as on 2025-09-07
  rev = "8eb28adfa3dc4de28e792e3bf49fcf9007ca8ac9";

  sha256 = "sha256:0vi9nycvag2a9142n6gi5migp7fl5wdzsi1xk8p8542ki2sd9sil";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

in
import nixpkgs {
  config = { allowUnfree = true; };
  overlays = [
    (import ./overlays/haskell-packages.nix)
  ];
}
