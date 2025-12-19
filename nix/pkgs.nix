{ compiler ? "ghc910" }:
let
  # "nixos-unstable" as on 2025-11-12
  rev = "9da7f1cf7f8a6e2a7cb3001b048546c92a8258b4";

  sha256 = "sha256:04h7cq8rp8815xb4zglkah4w6p2r5lqp7xanv89yxzbmnv29np2a";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

in
import nixpkgs {
  config = { allowUnfree = true; };
  overlays = [
    (import ./overlays/haskell-packages.nix { inherit compiler; })
  ];
}
