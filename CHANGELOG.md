# Changelog

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
