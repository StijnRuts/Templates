{
  pkgs,
  lib,
  config,
  ...
}:
let
  DOMAIN = "example.localhost";
in
{
  certificates = [ DOMAIN ];

  env = {
    DB_HOST = "localhost";
    DB_PORT = toString config.processes.postgres.ports.db.value;
    DB_NAME = "devdb";
    DB_USER = "devuser";
    DB_PASSWORD = "password";
  };

  packages = with pkgs; [
    php83Packages.php-cs-fixer
    php84Packages.psalm
    deployer
  ];

  scripts = {
    "style:check".exec = "php-cs-fixer check src";
    "style:fix".exec = "php-cs-fixer fix src";
    "code:check".exec = "psalm --show-info=true";
    "code:fix".exec = "psalm --alter --issues=all";
    "tests".exec = "./vendor/bin/phpunit tests";
    "deploy".exec = "dep deploy";
  };

  languages.php = {
    enable = true;
    version = "8.4";
    extensions = [ "xdebug" ];
    ini = ''
      memory_limit = 256M
    '';
    fpm.pools.web = {
      settings = {
        "clear_env" = "no";
        "pm" = "dynamic";
        "pm.max_children" = 5;
        "pm.start_servers" = 2;
        "pm.min_spare_servers" = 1;
        "pm.max_spare_servers" = 5;
      };
    };
  };

  services.postgres = {
    enable = true;
    package = pkgs.postgresql_16;
    listen_addresses = config.env.DB_HOST;
    port = lib.toInt config.env.DB_PORT;
    initialDatabases = [
      {
        name = config.env.DB_NAME;
        user = config.env.DB_USER;
        pass = config.env.DB_PASSWORD;
        schema = ./schema.sql;
      }
    ];
  };

  processes.postgres = {
    ports.db.allocate = 5432;
  };

  services.caddy = {
    enable = true;
    virtualHosts.${DOMAIN} = {
      extraConfig = ''
        tls ${config.env.DEVENV_STATE}/mkcert/${DOMAIN}.pem ${config.env.DEVENV_STATE}/mkcert/${DOMAIN}-key.pem
        root * public
        php_fastcgi unix/${config.languages.php.fpm.pools.web.socket}
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
      deadnix.enable = true;
      statix.enable = true;
      prettier.enable = true;
    };
  };

  git-hooks.hooks = {
    treefmt.enable = true;
    typos.enable = true;
    markdownlint.enable = true;
    php-cs-fixer.enable = true;
    phpcs.enable = true;
    psalm.enable = true;
  };
}
