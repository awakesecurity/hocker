{ compiler ? "ghc865" }:

let
  fetchNixpkgs = import ./nix/fetchNixpkgs.nix;

  # On branch "release-20.03":
  nixpkgs = fetchNixpkgs {
    rev    = "fdfd5ab05444c38a006cb107d7d1ee8cb0b15719";
    sha256 = "17hsjpjahl0hff3z2khrcwxygjyyrav2pia3qqlli0sgywfrgf95";
  };

  config = { allowUnfree = true; };

  overlays = [
    (newPkgs: oldPkgs: rec {

      haskell = oldPkgs.haskell // {
        packages = oldPkgs.haskell.packages // {
          "${compiler}" = oldPkgs.haskell.packages."${compiler}".override {
            overrides =
              let
                packageSourceOverrides = oldPkgs.haskell.lib.packageSourceOverrides {
                  "hocker" = ./.;
                };

                manualOverrides = haskellPackagesNew: haskellPackagesOld: {
                  neat-interpolation =
                    haskellPackagesNew.callPackage ./nix/neat-interpolation.nix { };
                };

              in
                newPkgs.lib.composeExtensions
                  packageSourceOverrides
                  manualOverrides;
          };
        };
      };
    })
  ];

  pkgs = import nixpkgs { inherit config overlays; };

in

  { inherit (pkgs.haskell.packages."${compiler}") hocker; }
