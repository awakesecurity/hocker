pkgsFinal: pkgsPrev:

let
  extension = pkgsPrev.haskell.lib.packageSourceOverrides {
    hocker = pkgsPrev.nix-gitignore.gitignoreSource [ ] ../../.;
  };

in
{
  haskellPackages =
    pkgsPrev.haskellPackages.override (old: {
      overrides =
        pkgsPrev.lib.fold
          pkgsPrev.lib.composeExtensions
          (old.overrides or (_: _: { }))
          [
            extension
          ];
    });
}
