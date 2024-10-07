# Changelog

## Unreleased
### Changed
- Breaking: Data.Docker.Nix.FetchDocker.inheritAdapter no longer
  supports dynamic keys because Nix-Expr-Types.Binding.Inherit
  no longer supports them as of hnix-0.15.
- Support hnix-0.15, hnix-0.16, and hnix-0.17.
- Support and require optparse-generic-1.4.0 or higher.
- Support turtle-1.6 and turtle
- Add the `--credentials-file` option to allow passing credentials as a file.

## 1.0.7
### Changed
- Support and require hnix-0.9.0 or higher.

## 1.0.6
### Changed
- Support and require hnix-0.7.0 or higher.
- Support GHC 8.10.1.
- Use nixpkgs 20.03 in Nix builds.
- Discard support for Nix versions < 1.12.

## 1.0.5
### Fixed
- Add the golden test data to the source distribution by using `data-files`
  instead of the incorrect `data-dir`, this fixes the problem of `cabal test`
  failing when run outside of the repository root on an unpacked tarball of the
  source distribution from Hackage
- The failing response integrity check in `hocker-image` by removing the check,
  the fix and its rationale are exactly the same as applied to `hocker-config`
  and `hocker-layer` in the `1.0.2` release
- Incorrect serialization of the registry URI in the `fetchImage` function,
  resulting in a malformed repository string that `docker load` would fail to
  import

## 1.0.4
### Changed
- Switched to the `nix-paths` library which provides compile-time constants for
  the Nix executables instead of relying on findexec and hoping Nix is on the
  PATH

## 1.0.3
### Added
- `readField` implementations for the custom `ParseField` instances to
  accommodate a minor API change in `optparse-generic` version `1.2.3`

## 1.0.2
### Fixed

- `hocker-config` and `hocker-layer` fail the response integrity check (#23)
- Missing `test/data` directory in cabal source distribution

## 1.0.1
### Fixed

- `hocker-manifest` said it accepted the manifest list media type when
  requesting an image manifest for a docker image but it doesn't know what to do
  with that, the manifest list accepts string has been removed

## 1.0.0
- Initial release
