{
  config,
  pkgs,
  lib,
  ...
}:
let
  DOMAIN = "example.localhost";
in
{
  certificates = [
    DOMAIN
    ("www." + DOMAIN)
  ];

  env = {
    DB_HOST = "localhost";
    DB_PORT = toString config.processes.mysql.ports.db.value;
    DB_NAME = "devdb";
    DB_USER = "devuser";
    DB_PASSWORD = "password";
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

  services.mysql = {
    enable = true;
    package = pkgs.mariadb_114;
    settings.mysqld = {
      port = lib.toInt config.env.DB_PORT;
      max_allowed_packet = "512M";
    };
    initialDatabases = [
      {
        name = config.env.DB_NAME;
        schema = ./schema.sql;
      }
    ];
    ensureUsers = [
      {
        name = config.env.DB_USER;
        password = config.env.DB_PASSWORD;
        ensurePermissions = {
          "${config.env.DB_NAME}.*" = "ALL PRIVILEGES";
        };
      }
    ];
  };

  processes.mysql = {
    ports.db.allocate = 3306;
  };

  services.apache = {
    enable = true;
    hostname = DOMAIN;
    httpPort = config.processes.apache.ports.http.value;
    httpsPort = config.processes.apache.ports.https.value;
    sslCert = "${config.env.DEVENV_STATE}/mkcert/${DOMAIN}+1.pem";
    sslKey = "${config.env.DEVENV_STATE}/mkcert/${DOMAIN}+1-key.pem";
    phpSocket = "${config.languages.php.fpm.pools.web.socket}";
  };

  processes.apache = {
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
