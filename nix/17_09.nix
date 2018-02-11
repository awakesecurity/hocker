let
  fetchNixpkgs = import ./fetchNixpkgs.nix;

in
  fetchNixpkgs {
     rev          = "3389f23412877913b9d22a58dfb241684653d7e9";
     sha256       = "1zf05a90d29bpl7j56y20r3kmrl4xkvg7gsfi55n6bb2r0xp2ma5";
     outputSha256 = "0wgm7sk9fca38a50hrsqwz6q79z35gqgb9nw80xz7pfdr4jy9pf8";
  }
