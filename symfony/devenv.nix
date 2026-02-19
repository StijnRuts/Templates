{
  pkgs,
  lib,
  config,
  ...
}:
let
  DOMAIN = "symfony.localhost";
in
{
  certificates = [ DOMAIN ];

  dotenv.enable = true;

  env.DB_PORT = "5454"; # toString config.processes.postgres.ports.db.value;
  env.SMTP_PORT = "1025"; # toString config.processes.mailpit.ports.smtp.value;
  env.SMTP_UI_PORT = "8025"; # toString config.processes.mailpit.ports.ui.value;
  env.DATABASE_URL = "postgresql://${config.env.DB_USER}:${config.env.DB_PASSWORD}@${config.env.DB_HOST}:${config.env.DB_PORT}/${config.env.DB_NAME}?serverVersion=16&charset=utf8";
  env.MAILER_DSN = "smtp://${config.env.SMTP_USER}:${config.env.SMTP_PASSWORD}@${config.env.SMTP_HOST}:${config.env.SMTP_PORT}";

  packages = with pkgs; [
    symfony-cli
  ];

  languages.php = {
    enable = true;
    version = "8.4";
    extensions = [
      "xdebug"
      "ctype"
      "iconv"
      "session"
      "simplexml"
      "tokenizer"
    ];
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
    listen_addresses = "localhost";
    port = lib.toInt config.env.DB_PORT;
    initialDatabases = [
      {
        name = config.env.DB_NAME;
        user = config.env.DB_USER;
        pass = config.env.DB_PASSWORD;
        # schema = ./schema.sql;
      }
    ];
  };

  processes.postgres = {
    ports.db.allocate = 5432;
  };

  services.mailpit = {
    enable = true;
    smtpListenAddress = "127.0.0.1:${config.env.SMTP_PORT}";
    uiListenAddress = "127.0.0.1:${config.env.SMTP_UI_PORT}";
  };

  processes.mailpit = {
    ports.smtp.allocate = 1025;
    ports.ui.allocate = 8025;
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
