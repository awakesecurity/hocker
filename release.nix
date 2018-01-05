let
  config = { allowUnfree = true;
    packageOverrides = pkgs: {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {
          optparse-applicative =
            pkgs.haskell.lib.dontCheck
              (haskellPackagesNew.callPackage ./nix/optparse-applicative.nix { });

          optparse-generic =
            haskellPackagesNew.callPackage ./nix/optparse-generic.nix { };

          turtle =
            haskellPackagesNew.callPackage ./nix/turtle.nix { };

          http-client =
            haskellPackagesNew.callPackage ./nix/http-client.nix { };

          http-client-tls =
            haskellPackagesNew.callPackage ./nix/http-client-tls.nix { };

          Only =
            haskellPackagesNew.callPackage ./nix/Only.nix { };

          hocker =
            haskellPackagesNew.callPackage ./default.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
in
  { inherit (pkgs.haskellPackages) hocker; }
