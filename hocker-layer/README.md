# Retrieve an individual docker image layer

## Quickstart

```shell
Fetch a docker image layer from a docker registry without using docker

Usage: hocker-layer [--registry URI]
                    [(-u|--username BASIC USERNAME)
                      (-p|--password BASIC PASSWORD) |
                      (-t|--token BEARER TOKEN) | (-f|--credentials-file PATH)]
                    [--out STRING] (-l|--layer SHA256) IMAGE-NAME IMAGE-TAG

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
  -l,--layer SHA256        Hash digest of layer to fetch
  IMAGE-NAME               Docker image name, e.g: 'debian' in debian:jessie
  IMAGE-TAG                Docker image tag identifier, e.g: 'jessie' in
                           debian:jessie
```
