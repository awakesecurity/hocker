# Fetch a docker image

## Quickstart

```shell
Fetch a docker image from a docker registry without using docker

Usage: hocker-image [--registry URI] ([-u|--username BASIC USERNAME]
                    [-p|--password BASIC PASSWORD] | [-t|--token BEARER TOKEN])
                    [--out STRING] IMAGE-NAME IMAGE-TAG

Available options:
  -h,--help                Show this help text
  --registry URI           URI of registry, defaults to the Docker Hub registry
  -u,--username BASIC USERNAME
                           Username part of a basic auth credential
  -p,--password BASIC PASSWORD
                           Password part of a basic auth credential
  -t,--token BEARER TOKEN  Bearer token retrieved from a call to `docker login`
                           (mutually exclusive to --username and --password)
  --out STRING             Write content to location
  IMAGE-NAME               Docker image name, e.g: 'debian' in debian:jessie
  IMAGE-TAG                Docker image tag identifier, e.g: 'jessie' in
                           debian:jessie
```

```shell
$ hocker-image --out=./debian-latest.tar.gz library/debian latest
Downloading layer: 22def84 => decompressed => wrote 159fbd8
./debian-latest.tar.gz
```
