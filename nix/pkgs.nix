let
  # "nixos-unstable" as on 2024-12-02
  rev = "ac35b104800bff9028425fec3b6e8a41de2bbfff";

  sha256 = "sha256:1fbj7shlmviilmgz5z2gp59j6xwgdr01jfh75qhixx06kib4305p";

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
