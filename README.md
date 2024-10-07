# Welcome!
The `hocker` package provides a small set of utilities to fetch docker image
artifacts from docker registries and produce Nix derivations marrying docker and
Nix elegantly:

- [`hocker-image`](./hocker-image/README.md) for fetching a docker image
- [`hocker-layer`](./hocker-layer/README.md) for fetching a docker image's layers
- [`hocker-config`](./hocker-config/README.md) for fetching a docker image's configuration JSON
- [`hocker-manifest`](./hocker-manifest/README.md) for fetching a docker registry image manifest
- [`docker2nix`](./docker2nix/README.md) for generating Nix expressions calling the `fetchdocker`
  derivations, given a docker registry image manifest
  
These tools _only_ work with version 2 of the **docker registry** and **docker
(>=) v1.10**.

The motivation for this tool came from a need to fetch docker image artifacts
from a docker registry without the stock docker tooling designed to only work
with the docker daemon.

Our use case (and the reason why this package exposes a `docker2nix` tool) is pulling
docker images into a [NixOS system's store](https://nixos.org/nix/manual/#ch-about-nix) and 
loading those images from the store into the docker daemon running on that same system.

We desired this for two critical reasons:
1. The docker daemon no longer required an internet connection in order to load
   the docker images
2. By virtue of fetching the docker images at build-time as opposed to run-time,
   failures from non-existent images or image tags are caught earlier

We strived to make this tool useful outside of the context of Nix and NixOS,
therefore all of these tools are usable without Nix in the workflow.

For high-level documentation of each utility, please refer to the README's in
their respective directories (links are in the above list).

## Quickstart
Let's first retrieve a docker registry image manifest for the `debian:jessie`
docker image (note that we need the `library/` repository prefix because we are
pulling from the official debian repository!):

```shell
$ hocker-manifest library/debian jessie
{
   "schemaVersion": 2,
   "mediaType": "application/vnd.docker.distribution.manifest.v2+json",
   "config": {
      "mediaType": "application/vnd.docker.container.image.v1+json",
      "size": 1528,
      "digest": "sha256:054abe38b1e6f863befa4258cbfaf127b1cc9440d2e2e349b15d22e676b591e7"
   },
   "layers": [
      {
         "mediaType": "application/vnd.docker.image.rootfs.diff.tar.gzip",
         "size": 52550276,
         "digest": "sha256:cd0a524342efac6edff500c17e625735bbe479c926439b263bbe3c8518a0849c"
      }
   ]
}
```

Next, we can easily generate a `fetchdocker` derivation using `docker2nix`:

```shell
$ hocker-manifest library/debian jessie | docker2nix library/debian jessie
{ fetchDockerConfig, fetchDockerLayer, fetchdocker }:
fetchdocker rec {
    name = "debian";
    registry = "https://registry-1.docker.io/v2/";
    repository = "library";
    imageName = "debian";
    tag = "jessie";
    imageConfig = fetchDockerConfig {
      inherit tag registry repository imageName;
      sha256 = "1rwinmvfc8jxn54y7qnj82acrc97y7xcnn22zaz67y76n4wbwjh5";
    };
    imageLayers = let
      layer0 = fetchDockerLayer {
        inherit registry repository imageName;
        layerDigest = "cd0a524342efac6edff500c17e625735bbe479c926439b263bbe3c8518a0849c";
        sha256 = "1744l0c8ag5y7ck9nhr6r5wy9frmaxi7xh80ypgnxb7g891m42nd";
      };
      in [ layer0 ];
  }
```

## Private registries
We developed these tools with private registries in-mind and they currently
support three modes of authentication:

1. Nothing at all (simply do not supply `--token` or `--username` and
   `--password`)
2. Bearer token-based authentication, you should retrieve a token and then give
   it via the `--token` flag
3. Basic authentication with `--username` and `--password` (most common with
   nginx proxied registries providing basic auth protection; you should be
   careful to ensure you're only sending requests to registries exposed via TLS
   or SSL!)

A caveat to #1 if you do not supply any authentication credential flags and you
also do not supply a `--registry` flag then the tools assume you wish to make a
request to the public docker hub registry, in which case they ask for a
short-lived authentication token from the registry auth server and then make the
request to the public docker hub registry.

Both types of credential may instead be passed as file using `--credential-file`.
The credential file should contain either:
```
USERNAME=<username>
PASSWORD=<password>
```
or
```
BEARER_TOKEN=<token>
```

## How to build

Building (and developing a patch for) this project using `cabal` is
straight-forward if we have Nix installed:

```bash
$ nix-shell
[nix-shell:]$ cabal --version
cabal-install version 1.24.0.2
compiled using version 1.24.2.0 of the Cabal library
```

... `cabal` and all of the package dependencies will be in the shell environment
so that we can then:

```bash
[nix-shell:]$ cabal build
```

Alternatively we can `nix-build` the project, this is not recommended for
development because Nix will not build the project incrementally:

```bash
$ nix-build --attr hocker release.nix
these derivations will be built:
  /nix/store/3dwvcm66360fpfqrrc4swp9y4q0jzvh9-hocker-0.1.0.0.drv
building path(s) ‘/nix/store/g16mrfhlmb1z3qkdzr0diaqn2dhl8bv6-hocker-0.1.0.0’
...
```
