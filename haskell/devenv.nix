{ pkgs, ... }:
{
  languages.haskell = {
    enable = true;
    package = pkgs.haskell.packages.ghc9103.ghcWithPackages (ps: with ps; [
      hspec
      QuickCheck
    ]);
  };

  scripts = {
    build.exec = "mkdir -p output && ghc -Wall -outputdir output -o output/main -i=src src/Main.hs";
    run.exec = "ghc -Wall -i=src --run src/Main.hs";
    tests.exec = "ghc -Wall -i=src -i=test --run test/Main.hs";
    format.exec = "ormolu --mode inplace $(find {src,test} -name '*.hs')";
    lint.exec = "hlint {src,test}";
  };

  git-hooks.hooks = {
    ormolu.enable = true;
    hlint.enable = true;
  };
}
