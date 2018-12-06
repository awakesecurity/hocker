let

  config = { allowUnfree = true; };

  overlays = [
    (newPkgs: oldPkgs: {

      haskellPackages = oldPkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          hnix =
            newPkgs.haskell.lib.dontCheck
              (haskellPackagesNew.callPackage ./nix/hnix.nix { });

          hocker = haskellPackagesNew.callPackage ./default.nix { };

          megaparsec = haskellPackagesNew.callPackage ./nix/megaparsec.nix { };

          neat-interpolation = haskellPackagesNew.callPackage ./nix/neat-interpolation.nix { };
        };
      };

    })
  ];

  nixpkgs = import ./nix/18_09.nix;

  pkgs = import nixpkgs { inherit config overlays; };

in
  { inherit (pkgs.haskellPackages) hocker; }
