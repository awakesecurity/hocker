let
  # master as of 2022-05-16
  rev = "72cb3780a57881aa2fc5e2268380d85de7ea2b4c";

  sha256 = "sha256-NNuTF0Z2jHErDr0JLXd+h996xPXGS9cbBBJDENUf13o=";

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
