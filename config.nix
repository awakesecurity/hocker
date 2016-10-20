{ allowUnfree = true;
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

        wreq =
          haskellPackagesNew.callPackage ./nix/wreq.nix { };

        http-client =
          haskellPackagesNew.callPackage ./nix/http-client.nix { };

        http-client-tls =
          haskellPackagesNew.callPackage ./nix/http-client-tls.nix { };

        hocker =
          pkgs.haskell.lib.overrideCabal
            ( haskellPackagesNew.callPackage ./default.nix { } )
            ( oldDerivation: {
                testToolDepends =
                  (oldDerivation.testToolDepends or []) ++[ pkgs.nix ];
                buildDepends    =
                  (oldDerivation.buildDepends or []) ++ [ pkgs.makeWrapper ];
    
                postInstall     =
                  (oldDerivation.postInstall or "") + ''
                    wrapProgram $out/bin/hocker-* \
                      --suffix PATH : ${pkgs.nix}/bin

                    wrapProgram $out/bin/docker2nix \
                      --suffix PATH : ${pkgs.nix}/bin
                  '';
              }
            );
      };
    };
  };
}
