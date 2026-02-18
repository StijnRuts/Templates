{ lib, pkgs, ... }:
{
  scripts = {
    "build:frontend".exec = "pushd $DEVENV_ROOT/frontend; npm run build; popd";
    "build:prod:frontend".exec = "pushd $DEVENV_ROOT/frontend; npm run build:prod; popd";
    "test:frontend".exec = "pushd $DEVENV_ROOT/frontend; npm run test; popd";
  };

  processes = {
    "build:watch:frontend".exec = "cd $DEVENV_ROOT/frontend; npm run build:watch";
    "test:watch:frontend".exec = "cd $DEVENV_ROOT/frontend; npm run test:watch";
  };

  treefmt.config.settings.formatter = {
    "eslint-frontend" = {
      command = "${lib.getExe pkgs.eslint}";
      options = [ "--config frontend/eslint.config.mjs" ];
      includes = [ "frontend/**/*.js" ];
    };
  };
}
