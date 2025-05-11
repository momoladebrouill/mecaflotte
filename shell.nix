{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixpkgs-unstable.tar.gz") {} }:

pkgs.mkShell {
  buildInputs = with pkgs.ocamlPackages; [
    ocaml
    raylib
    findlib
    dune_3
  ];
}

