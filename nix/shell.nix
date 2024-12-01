{
  pkgs ? import <nixpkgs> { },
}:

pkgs.mkShell {
  packages = with pkgs; [
    ocaml
    opam
    lefthook
    just
  ];

  shellHook = ''
    ${pkgs.just}/bin/just setup
  '';
}
