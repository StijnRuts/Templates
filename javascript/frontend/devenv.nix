{
  scripts = {
    "build:frontend".exec = "pushd $DEVENV_ROOT/frontend; npm run build; popd";
    "build:watch:frontend".exec = "pushd $DEVENV_ROOT/frontend; npm run build:watch; popd";
    "build:prod:frontend".exec = "pushd $DEVENV_ROOT/frontend; npm run build:prod; popd";
    "format:frontend".exec = "pushd $DEVENV_ROOT/frontend; npm run format; popd";
    "lint:frontend".exec = "pushd $DEVENV_ROOT/frontend; npm run lint; popd";
    "test:frontend".exec = "pushd $DEVENV_ROOT/frontend; npm run test; popd";
    "test:watch:frontend".exec = "pushd $DEVENV_ROOT/frontend; npm run test:watch; popd";
  };
}
