{ pkgs, ... }:
{
  languages.haskell = {
    enable = true;
    package = pkgs.haskell.compiler.ghc9103;
  };

  scripts = {
    build.exec = "cabal build";
    run.exec = "cabal run";
    tests.exec = "cabal test";
    format.exec = "ormolu --mode inplace $(find {app,test} -name '*.hs')";
    lint.exec = "hlint {app,test}";
  };

  git-hooks.hooks = {
    ormolu.enable = true;
    hlint.enable = true;
  };
}
