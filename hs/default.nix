{ nixpkgs ? import <nixpkgs> {}, compiler ? "lts-5_8" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./chris-martin-org.nix { }
