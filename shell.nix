{ pkgs ? import <nixpkgs> {} }:
with pkgs;
with stdenv;
let
  elm-make = elmPackages.elm-make;
in
mkDerivation {
  name = "bujo";
  buildInputs = [ gcc mongodb elm-make ];
}
