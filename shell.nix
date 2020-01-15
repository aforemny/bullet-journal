{ pkgs ? import <nixpkgs> {}
, nixops ? import ./nixops.nix {}
}:
with pkgs;
with stdenv;
mkDerivation {
  name = "bujo";
  buildInputs = [
    elmPackages.elm
    gcc
    mongodb
    #nixops
    nodejs
    python
    sassc
  ];
  shellHook = ''
    export NIXOPS_STATE=deployments.nixops
    export NIX_PATH=production.nix=./nixops/production.nix:$NIX_PATH
    export NIX_PATH=development.nix=./nixops/development.nix:$NIX_PATH
    export NIX_PATH=server-hetzner.nix=./nixops/server-hetzner.nix:$NIX_PATH
    export NIX_PATH=server-virtualbox.nix=./nixops/server-virtualbox.nix:$NIX_PATH
    export HETZNER_CLOUD_AUTH_TOKEN=$(cat ./keys/authToken)
    export PATH=./node_modules/.bin:$PATH
  '';
}
