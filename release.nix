{ compiler ? "ghc865" }:

let
  fetchNixpkgs = import ./nix/fetchNixpkgs.nix;

  nixpkgsRelease = "20.03";
  unpatchedNixpkgs = fetchNixpkgs {
    rev    = "fdfd5ab05444c38a006cb107d7d1ee8cb0b15719";
    sha256 = "17hsjpjahl0hff3z2khrcwxygjyyrav2pia3qqlli0sgywfrgf95";
  };

  config = { allowUnfree = true; };

  upgrade = packageList: haskellPackagesNew: haskellPackagesOld:
    let op = name: {
          inherit name;
          value = haskellPackagesNew.callPackage (./nix + "/${name}.nix") { };
        };
    in
      builtins.listToAttrs (builtins.map op packageList);

  dontCheck = haskell: packageList: haskellPackagesNew: haskellPackagesOld:
    let op = name: {
          inherit name;
          value = haskell.lib.dontCheck haskellPackagesOld.${name};
        };
    in
      builtins.listToAttrs (builtins.map op packageList);

  jailbreak = haskell: packageList: haskellPackagesNew: haskellPackagesOld:
    let op = name: {
          inherit name;
          value = haskell.lib.doJailbreak haskellPackagesOld.${name};
        };
    in
      builtins.listToAttrs (builtins.map op packageList);

  composeExtensionList = lib: lib.foldr lib.composeExtensions (_: _: {});

  overlays = [
    (newPkgs: oldPkgs: rec {

      haskell = oldPkgs.haskell // {
        packages = oldPkgs.haskell.packages // {
          "${compiler}" = oldPkgs.haskell.packages.${compiler}.override {
            overrides =
              let
                packageSourceOverrides =
                  oldPkgs.haskell.lib.packageSourceOverrides {
                    "hocker" = ./.;
                  };

                upgradeOverrides = upgrade
                  ( [ "assoc"
                      "cassava"
                        "cborg"
                        "ChasingBottoms"
                        "comonad"
                        "constraints"
                        "contravariant-extras"
                        "cryptohash-sha256"
                        "cryptohash-sha512"
                        "Diff"
                        "distributive"
                        "doctest"
                        "haskell-src-exts"
                        "haskell-src-meta"
                        "hedgehog"
                        "hnix"
                        "hnix-store-core"
                        "language-haskell-extract"
                        "lens"
                        "lens-aeson"
                        "lens-family-th"
                        "lifted-async"
                        "memory"
                        "microstache"
                        "prettyprinter"
                        "quickcheck-instances"
                        "regex-base"
                        "regex-posix"
                        "regex-tdfa"
                        "RSA"
                        "saltine"
                        "semialign"
                        "semialign-indexed"
                        "semigroupoids"
                        "serialise"
                        "some"
                        "tasty-hedgehog"
                        "th-expand-syns"
                        "these"
                        "tls"
                        "turtle"
                        "vector-binary-instances"
                        "vector-th-unbox"
                    ] ++ (
                      let v = oldPkgs.haskell.packages.${compiler}.ghc.version;
                      in if builtins.compareVersions v "8.10.1" < 0
                           then []
                           else [ "repline" ]
                    )
                  );

                dontCheckOverrides = dontCheck haskell
                  [ "doctest"
                    "cryptohash-sha256"
                    "cryptohash-sha512"
                    "hnix"
                    "saltine"
                  ];

                jailbreakOverrides = jailbreak haskell
                  [ "cryptohash-sha512"
                    "language-haskell-extract"
                    "quickcheck-instances"
                    "semialign-indexed"
                    "system-fileio"
                    "tasty-hspec"
                  ];

                manualOverrides = haskellPackagesNew: haskellPackagesOld: {
                  # <https://github.com/finnsson/template-helper/issues/12>
                  language-haskell-extract =
                    newPkgs.haskell.lib.appendPatch
                      haskellPackagesOld.language-haskell-extract
                      ./nix/language-haskell-extract.patch;
                };

              in
                composeExtensionList newPkgs.lib
                  [ packageSourceOverrides
                    upgradeOverrides
                    dontCheckOverrides
                    jailbreakOverrides
                    manualOverrides
                  ];
          };
        };
      };
    })
  ];

  unpatchedPkgs = import unpatchedNixpkgs { inherit config overlays; };

  # https://github.com/NixOS/nixpkgs/pull/85446
  nixpkgs = unpatchedPkgs.stdenvNoCC.mkDerivation {
    name = "nixpkgs-${nixpkgsRelease}-patched";

    src = unpatchedNixpkgs;

    # Backport fix <https://github.com/NixOS/nixpkgs/pull/85446> to 20.03:
    patches = [ ./nix/with-packages-wrapper.patch ];

    phases = [ "unpackPhase" "patchPhase" "installPhase" ];

    installPhase = ''
      mkdir -p $out
      cp -R ./ $out/
    '';

    preferLocalBuild = true;
  };

  pkgs = import nixpkgs { inherit config overlays; };

in

  { inherit (pkgs.haskell.packages."${compiler}") hocker; }
