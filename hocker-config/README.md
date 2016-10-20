# Retrieve a docker image configuration JSON
This tool fetches the specified docker image's configuration JSON from the
docker registry.

## Quickstart
```shell
Fetch a docker image config JSON from the registry

Usage: hocker-config [--registry URI] ([-u|--username BASIC USERNAME]
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
$ hocker-config library/debian jessie | jq
{
  "architecture": "amd64",
  "config": {
    "Hostname": "200591939db7",
    "Domainname": "",
    "User": "",
    "AttachStdin": false,
    "AttachStdout": false,
    "AttachStderr": false,
    "Tty": false,
    "OpenStdin": false,
    "StdinOnce": false,
    "Env": [
      "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
    ],
    "Cmd": [
      "/bin/bash"
    ],
    "ArgsEscaped": true,
    "Image": "sha256:9e77974e778cc6730c21889f33f2dcb141f9f632745ba2c3914dd62250ea93c9",
    "Volumes": null,
    "WorkingDir": "",
    "Entrypoint": null,
    "OnBuild": null,
    "Labels": {}
  },
  "container": "9a3fb25551fee47cea1203cbc2a6022dc3ffea8bc2010733e1286c4702cdf778",
  "container_config": {
    "Hostname": "200591939db7",
    "Domainname": "",
    "User": "",
    "AttachStdin": false,
    "AttachStdout": false,
    "AttachStderr": false,
    "Tty": false,
    "OpenStdin": false,
    "StdinOnce": false,
    "Env": [
      "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
    ],
    "Cmd": [
      "/bin/sh",
      "-c",
      "#(nop) ",
      "CMD [\"/bin/bash\"]"
    ],
    "ArgsEscaped": true,
    "Image": "sha256:9e77974e778cc6730c21889f33f2dcb141f9f632745ba2c3914dd62250ea93c9",
    "Volumes": null,
    "WorkingDir": "",
    "Entrypoint": null,
    "OnBuild": null,
    "Labels": {}
  },
  "created": "2017-05-08T23:28:15.327579341Z",
  "docker_version": "17.04.0-ce",
  "history": [
    {
      "created": "2017-05-08T23:28:14.437236885Z",
      "created_by": "/bin/sh -c #(nop) ADD file:f4e6551ac34ab446a297849489a5693d67a7e76c9cb9ed9346d82392c9d9a5fe in / "
    },
    {
      "created": "2017-05-08T23:28:15.327579341Z",
      "created_by": "/bin/sh -c #(nop)  CMD [\"/bin/bash\"]",
      "empty_layer": true
    }
  ],
  "os": "linux",
  "rootfs": {
    "type": "layers",
    "diff_ids": [
      "sha256:8d4d1ab5ff74fc361fb74212fff3b6dc1e6c16d1e1f0e8b44f9a9112b00b564f"
    ]
  }
}
```


