{ pkgs ? import <nixpkgs> {} }:
with pkgs;
with stdenv;
let
  elm-make = elmPackages.elm-make;
in
mkDerivation {
  name = "bujo";
  buildInputs = [ elm-make gcc mongodb nodejs-8_x python sassc ];
}
