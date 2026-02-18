{
  config,
  lib,
  pkgs,
  ...
}:
{
  processes.backend = {
    ports.http.allocate = 3000;
    exec = "node $DEVENV_ROOT/backend/src/server.js";
  };

  env = {
    BACKEND_PORT = config.processes.backend.ports.http.value;
  };

  scripts = {
    "test:backend".exec = "pushd $DEVENV_ROOT/backend; npm run test; popd";
  };

  processes = {
    "test:watch:backend".exec = "cd $DEVENV_ROOT/backend; npm run test:watch";
  };

  treefmt.config.settings.formatter = {
    "eslint-backend" = {
      command = "${lib.getExe pkgs.eslint}";
      options = [ "--config backend/eslint.config.mjs" ];
      includes = [ "backend/**/*.js" ];
    };
  };
}
