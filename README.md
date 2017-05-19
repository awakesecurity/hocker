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
{
  config.docker.images.debian = pkgs.fetchdocker {
    name = "debian";
    registry = "https://registry-1.docker.io/v2/";
    repository = "library";
    imageName = "debian";
    tag = "jessie";
    imageConfig = pkgs.fetchDockerConfig {
      inherit registry repository imageName tag;
      sha256 = "1viqbygsz9547jy830f2lk2hcrxjf7gl9h1xda9ws5kap8yw50ry";
    };
    imageLayers = let
      layer0 = pkgs.fetchDockerLayer {
        inherit registry repository imageName tag;
        layerDigest = "10a267c67f423630f3afe5e04bbbc93d578861ddcc54283526222f3ad5e895b9";
        sha256 = "1fcmx3aklbr24qsjhm6cvmhqhmrxr6xlpq75mzrk0dj2gz36g8hh";
      };
      in [ layer0 ];
  };
}
```

## Private Registries
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

# TODO
- [X] ~Get a nix-build workflow working for hocker~
- [ ] Work on a nix-shell based dev workflow
- [ ] Document types in `Exceptions`, `ErrorHandling`, etc.
- [x] ~Rename the `Types/Extra.hs` module, that's poorly named~ (I got rid of it)
- [x] ~Write an updated and accurate README introduction~
- [X] Rename `ContainerName` and `ContainerTag` to `ImageName` and `ImageTag` to
  be more consistent with the correct docker terminology
- [x] ~Remove the run prefix from most of the `V1_2.hs` module functions~ (replaced with a `do` prefix)
- [X] ~Use HockerException in docker2nix's lib functions~
- [x] ~Better document the types and function signatures in `Nix/FetchDocker.hs`~
- [X] L258 fix docker-layer to hocker-layer
- [ ] Proofread comments
- [ ] `Data/Docker/Image/Types.hs` can probably move to a more general location
  I think
- [ ] Use friendly module prefixing more consistently and cleanup usage
- [ ] Strip out the unused docker image V1 code
