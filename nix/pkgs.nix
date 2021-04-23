let
  # master as of 2021-04-21
  rev = "fce51c84574b3a17f0cc53792189aeff4abc5663";

  sha256 = "10gp1567xcgaxajmzqm6svv2sxxl8c897c790vw9gw889pia6v4p";

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
