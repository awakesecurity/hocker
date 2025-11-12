{ compiler }:
pkgsFinal: pkgsPrev:

let
  extension = pkgsPrev.haskell.lib.packageSourceOverrides {
    hocker = pkgsPrev.nix-gitignore.gitignoreSource [ ] ../../.;
  };

in
{
  haskellPackages = 
    pkgsPrev.haskell.packages.${compiler}.override (old: {
      overrides =
        pkgsPrev.lib.fold
          pkgsPrev.lib.composeExtensions
          (old.overrides or (_: _: { }))
          [
            extension

            (haskellPackagesNew: haskellPackagesOld: {
              # hnix still depends on cryptonite and not crypton and the tests fail with GHC 9.12
              cryptonite = pkgsFinal.haskell.lib.compose.dontCheckIf (pkgsFinal.lib.versionAtLeast haskellPackagesNew.ghc.version "9.12") haskellPackagesOld.cryptonite;
            })
          ];
    });
}
