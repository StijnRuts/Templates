{ pkgs, lib, ... }:
{
  processes.backend.exec = "node $DEVENV_ROOT/backend/src/server.js";

  scripts = {
    "format:backend".exec = "pushd $DEVENV_ROOT/backend; npm run format; popd";
    "lint:backend".exec = "pushd $DEVENV_ROOT/backend; npm run lint; popd";
    "test:backend".exec = "pushd $DEVENV_ROOT/backend; npm run test; popd";
    "test:watch:backend".exec = "pushd $DEVENV_ROOT/backend; npm run test:watch; popd";
  };
}
