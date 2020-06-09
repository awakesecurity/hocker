{ mkDerivation, base, bifunctors, stdenv, tagged }:
mkDerivation {
  pname = "assoc";
  version = "1.0.1";
  sha256 = "4000dea2fbc272ff5a15a0bf0fae1c29dea4b87b8fb4ccb8b07b8114ee2636d5";
  revision = "1";
  editedCabalFile = "1q6sc9v79p2pdm7aa6mfbn824vc01wj267saf2gp86b3wzgp0mrh";
  libraryHaskellDepends = [ base bifunctors tagged ];
  description = "swap and assoc: Symmetric and Semigroupy Bifunctors";
  license = stdenv.lib.licenses.bsd3;
}
