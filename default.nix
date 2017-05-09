with import <nixpkgs> {};
stdenv.mkDerivation {
  name = "cryptosolver";
  version = "0.0.1";

  buildInputs = [ ocaml ocamlPackages.ocamlbuild ];

}
