{
  pkgs,
  lib,
  easy-purescript-nix,
  ...
}:
let
  easy-ps = easy-purescript-nix.packages.x86_64-linux;
in
{
  packages = [
    easy-ps.purs-0_15_15
    easy-ps.purescript-language-server
    easy-ps.spago
    easy-ps.purs-tidy
    easy-ps.psa
    easy-ps.pscid
    pkgs.esbuild
    pkgs.watchexec
  ];

  languages.purescript.enable = true;

  languages.javascript = {
    enable = true;
    package = pkgs.nodejs-slim_24;
    npm.enable = true;
  };

  scripts = {
    run.exec = "spago run";
    build.exec = "spago build";
    format.exec = "treefmt";
    repl.exec = "spago repl";
    interactive.exec = "pscid";
    docs.exec = "spago docs --format html --open";
    tests.exec = "spago test";
  };

  processes = {
    "build:watch".exec = "watchexec -c -w src -w test -e purs 'build && tests'";
    "format:watch".exec = "watchexec format";
    "parcel".exec = "parcel";
  };

  treefmt = {
    enable = true;
    config.programs = {
      nixfmt.enable = true;
      statix.enable = true;
      deadnix.enable = true;
      prettier.enable = true;
    };
  };

  treefmt.config.settings.formatter = {
    "purs-tidy" = {
      command = "${lib.getExe easy-ps.purs-tidy}";
      options = [ "format-in-place" ];
      includes = [ "*.purs" ];
    };
  };

  git-hooks.hooks = {
    treefmt.enable = true;
  };
}
