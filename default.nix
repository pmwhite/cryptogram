with import <nixpkgs-unstable> {};

let
  ocamlVersion = (builtins.parseDrvName ocamlPackages.ocaml.name).version;
  findlibSiteLib = "${ocamlPackages.findlib}/lib/ocaml/${ocamlVersion}/site-lib";
in
stdenv.mkDerivation {
  name = "cryptosolver";
  version = "0.0.1";

  buildInputs = with ocamlPackages;
  [ ocaml opam camlp4
    utop
    ocamlbuild ocamlgraph
    core findlib ];

  findlib = findlibSiteLib;

}
