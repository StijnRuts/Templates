{
  pkgs,
  lib,
  config,
  ...
}:
let
  DOMAIN = "wp.localhost";
in
{
  certificates = [ DOMAIN ];

  env = {
    DB_HOST = "localhost";
    DB_PORT = toString config.processes.mysql.ports.db.value;
    DB_NAME = "wp";
    DB_USER = "wordpress";
    DB_PASSWORD = "password";
  };

  packages = with pkgs; [
    wp-cli
  ];

  languages.php = {
    enable = true;
    package = pkgs.php82.buildEnv {
      extensions =
        { all, enabled }:
        with all;
        enabled
        ++ [
          redis
          pdo_mysql
          xdebug
        ];
      extraConfig = ''
        memory_limit = -1
        xdebug.mode = debug
        xdebug.start_with_request = yes
        xdebug.idekey = vscode
        xdebug.log_level = 0
        max_execution_time = 0
      '';
    };
    fpm.pools.web = {
      settings = {
        "clear_env" = "no";
        "pm" = "dynamic";
        "pm.max_children" = 10;
        "pm.start_servers" = 2;
        "pm.min_spare_servers" = 1;
        "pm.max_spare_servers" = 10;
      };
    };
  };

  services.mysql = {
    enable = true;
    settings.mysqld = {
      port = lib.toInt config.env.DB_PORT;
      max_allowed_packet = "512M";
    };
    initialDatabases = [ { name = config.env.DB_NAME; } ];
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

  services.redis.enable = true;

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
