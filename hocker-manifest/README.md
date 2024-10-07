# Retrieve a docker registry V2 image manifest
This utility retrieves a V2 docker image manifest from the docker registry.

NB: the V2 docker image manifest retrieved from the docker registry is a
manifest of the configuration JSON and layer blobs stored by the registry, this
is _not_ the same manifest JSON file of the docker image V1.2 _image_
specification.

## Quickstart

```shell
Usage: hocker-manifest [--registry URI]
                       [(-u|--username BASIC USERNAME)
                         (-p|--password BASIC PASSWORD) |
                         (-t|--token BEARER TOKEN) |
                         (-f|--credentials-file PATH)] [--out STRING] IMAGE-NAME
                       IMAGE-TAG

Available options:
  -h,--help                Show this help text
  --registry URI           URI of registry, defaults to the Docker Hub registry
  -u,--username BASIC USERNAME
                           Username part of a basic auth credential
  -p,--password BASIC PASSWORD
                           Password part of a basic auth credential
  -t,--token BEARER TOKEN  Bearer token retrieved from a call to `docker login`
                           (mutually exclusive to --username and --password and --credentials-file)
  -f,--credentials-file PATH
                           Path to a file containing either:

                           USERNAME=<username>
                           PASSWORD=<password>

                           or

                           BEARER_TOKEN=<token>

                           (mutually exclusive to --username and --password and --token)
  --out STRING             Write content to location
  IMAGE-NAME               Docker image name, e.g: 'debian' in debian:jessie
  IMAGE-TAG                Docker image tag identifier, e.g: 'jessie' in
                           debian:jessie
```

```shell
hocker-manifest library/debian jessie
{
   "schemaVersion": 2,
   "mediaType": "application/vnd.docker.distribution.manifest.v2+json",
   "config": {
      "mediaType": "application/vnd.docker.container.image.v1+json",
      "size": 1528,
      "digest": "sha256:3e83c23dba6a16cd936a3dc044df71b26706c5a4c28181bc3ca4a4af9f5f38ee"
   },
   "layers": [
      {
         "mediaType": "application/vnd.docker.image.rootfs.diff.tar.gzip",
         "size": 52584016,
         "digest": "sha256:10a267c67f423630f3afe5e04bbbc93d578861ddcc54283526222f3ad5e895b9"
      }
   ]
}
```
