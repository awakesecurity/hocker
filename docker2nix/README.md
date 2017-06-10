# Generate nix expression to fetch a docker image
This tool takes a docker registry V2 image manifest JSON on stdin or as a file
to read from and generates a Nix expression that uses the fetchdocker machinery
to pull all individual layers and generate an image compositor that can stream
to `docker load`.

## Quickstart

```shell
$ docker2nix --help
Produce a Nix expression given a manifest for a docker image via stdin or via a
filepath

Usage: docker2nix [--registry URI] [--manifest STRING] [--altName TEXT]
                  IMAGE-NAME IMAGE-TAG

Available options:
  -h,--help                Show this help text
  --registry URI           URI of registry, defaults to the Docker Hub registry
  --manifest STRING        Fetch image manifest from a path on the filesystem
  --altName TEXT           Alternate image name provided in the `fetcdocker`
                           derivation
  IMAGE-NAME               Docker image name, e.g: 'debian' in debian:jessie
  IMAGE-TAG                Docker image tag identifier, e.g: 'jessie' in
                           debian:jessie
```

Generating a fetchdocker Nix expression from a docker registry V2 image manifest
JSON retrieved by `hocker-manifest`:

```shell
$ hocker-manifest library/debian jessie | docker2nix library/debian jessie
{ fetchDockerConfig, fetchDockerLayer, fetchdocker }:
fetchdocker {
    name = "debian";
    registry = "https://registry-1.docker.io/v2/";
    repository = "library";
    imageName = "debian";
    tag = "jessie";
    imageConfig = fetchDockerConfig {
      inherit registry repository imageName tag;
      sha256 = "1rwinmvfc8jxn54y7qnj82acrc97y7xcnn22zaz67y76n4wbwjh5";
    };
    imageLayers = let
      layer0 = fetchDockerLayer {
        inherit registry repository imageName tag;
        layerDigest = "cd0a524342efac6edff500c17e625735bbe479c926439b263bbe3c8518a0849c";
        sha256 = "1744l0c8ag5y7ck9nhr6r5wy9frmaxi7xh80ypgnxb7g891m42nd";
      };
      in [ layer0 ];
  }
```

And to load a fetched docker image into a running docker daemon on a NixOS
system (NB the preferred method to do the below might be in a systemd unit with
the `config.docker.images.debian` attribute parametrizing the path to
`compositeImage.sh`):

```shell
$ /nix/store/6qn5i7p6x3c3qylvzqf76fqgd0gl47cv-debian/compositeImage.sh | docker load
```
