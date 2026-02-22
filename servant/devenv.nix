{ pkgs, config, ... }:
{
  languages.haskell = {
    enable = true;
    package = pkgs.haskell.packages.ghc9103.ghcWithPackages (
      ps: with ps; [
        blaze-html
        hspec
        hspec-wai
        lucid
        servant-server
        QuickCheck
      ]
    );
  };

  packages = with pkgs; [
    ghcid
    watchexec
  ];

  scripts = {
    build.exec = "mkdir -p output && ghc -Wall -outputdir output -o output/main -i=src src/Main.hs";
    run.exec = "ghc -Wall -i=src --run src/Main.hs -- '$@'";
    format.exec = "treefmt";
    tests.exec = "ghc -Wall -i=src -i=test --run test/Test/Main.hs";
  };

  processes = {
    "serve".exec = "watchexec -r -w output/main ./output/main";
    "build:watch".exec = "watchexec -c -w src -w test -e hs 'build && tests'";
    "format:watch".exec = "watchexec format";
  };

  processes.serve = {
    ports.http.allocate = 8080;
  };

  env.SERVANT_PORT = toString config.processes.serve.ports.http.value;

  treefmt = {
    enable = true;
    config.programs = {
      nixfmt.enable = true;
      statix.enable = true;
      deadnix.enable = true;
      prettier.enable = true;
      ormolu.enable = true;
      hlint.enable = true;
    };
  };

  git-hooks.hooks = {
    treefmt.enable = true;
  };
}
