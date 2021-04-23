let
  pkgs = import ./nix/pkgs.nix;

  hocker = pkgs.haskellPackages.hocker;

in
hocker.env.overrideAttrs (old: {
  buildInputs = (old.buildInputs or [ ]) ++ [
    pkgs.cabal-install
    pkgs.ghcid
  ];
})
