{
  pkgs,
  lib,
  easy-purescript-nix,
  config,
  ...
}:
let
  DOMAIN = "example.localhost";
  easy-ps = easy-purescript-nix.packages.x86_64-linux;
in
{
  certificates = [ DOMAIN ];

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
    bundle.exec = "spago bundle --outfile public/index.js --source-maps";
    format.exec = "treefmt";
    repl.exec = "spago repl";
    interactive.exec = "pscid";
    docs.exec = "spago docs --format html --open";
    tests.exec = "spago test";
  };

  processes = {
    "build:watch".exec = "watchexec -c -w src -w test -e purs 'build && tests'";
    "format:watch".exec = "watchexec format";
  };

  services.caddy = {
    enable = true;
    virtualHosts.${DOMAIN} = {
      extraConfig = ''
        tls ${config.env.DEVENV_STATE}/mkcert/${DOMAIN}.pem ${config.env.DEVENV_STATE}/mkcert/${DOMAIN}-key.pem
        root * public
        file_server
      '';
    };
    config = ''
      {
        admin off
        http_port ${toString config.processes.caddy.ports.http.value}
        https_port ${toString config.processes.caddy.ports.https.value}
      }
    '';
  };

  processes.caddy = {
    ports.http.allocate = 8080;
    ports.https.allocate = 8443;
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
