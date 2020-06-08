{ rev                             # The Git revision of nixpkgs to fetch
, sha256                          # The SHA256 hash of the unpacked archive
, system ? builtins.currentSystem # This is overridable if necessary
}:

# In Nix 1.12, we can just give a `sha256` to `builtins.fetchTarball`.
builtins.fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
  inherit sha256;
}
