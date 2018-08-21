{ pkgs ? import <nixpkgs> {} }:
with pkgs;
with stdenv;
with elmPackages;
mkDerivation {
  name = "bujo";
  buildInputs = [ elm-package elm-make gcc mongodb nodejs-9_x python sassc ];
}
