{pkgs, ...}: {
  languages.haskell = {
    enable = true;
    package = pkgs.haskell.packages.ghc9103.ghcWithPackages (ps:
      with ps; [
        hspec
        QuickCheck
      ]);
  };

  packages = with pkgs; [
    ghcid
  ];

  scripts = {
    build.exec = "mkdir -p output && ghc -Wall -outputdir output -o output/main -i=src src/Main.hs";
    run.exec = "ghc -Wall -i=src --run src/Main.hs -- '$@'";
    tests.exec = "ghc -Wall -i=src -i=test --run test/Test/Main.hs";
    watch.exec = "ghcid --test=Test.Main.main --lint=lint";
    format.exec = "ormolu --mode inplace $(find {src,test} -name '*.hs')";
    lint.exec = "hlint {src,test}";
    docs.exec = "haddock --html --no-warnings -o docs $(find src -name '*.hs')";
  };

  git-hooks.hooks = {
    ormolu.enable = true;
    hlint.enable = true;
  };
}
