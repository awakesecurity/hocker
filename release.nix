let

  config = { allowUnfree = true; };

  overlays = [
    (newPkgs: oldPkgs: {

      haskellPackages = oldPkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {

          optparse-applicative =
            newPkgs.haskell.lib.dontCheck
              (haskellPackagesNew.callPackage ./nix/optparse-applicative.nix { });

          optparse-generic =
            haskellPackagesNew.callPackage ./nix/optparse-generic.nix { };

          turtle =
            haskellPackagesNew.callPackage ./nix/turtle.nix { };

          nix-paths =
            haskellPackagesNew.callPackage ./nix/nix-paths.nix { };

          hocker =
            haskellPackagesNew.callPackage ./default.nix { };
        };
      };

    })
  ];

  nixpkgs = import ./nix/17_09.nix;

  pkgs = import nixpkgs { inherit config overlays; };

in
  { inherit (pkgs.haskellPackages) hocker; }
