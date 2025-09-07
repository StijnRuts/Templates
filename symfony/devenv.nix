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
  dotenv.enable = true;
  env.DATABASE_URL = "postgresql://${config.env.DB_USER}:${config.env.DB_PASSWORD}@${config.env.DB_HOST}:${config.env.DB_PORT}/${config.env.DB_NAME}?serverVersion=16&charset=utf8";
  env.MAILER_DSN = "smtp://${config.env.SMTP_USER}:${config.env.SMTP_PASSWORD}@${config.env.SMTP_HOST}:${config.env.SMTP_PORT}";

  certificates = [ DOMAIN ];

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
      # "pcre"
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

  services.mailpit = {
    enable = true;
    smtpListenAddress = "127.0.0.1:${config.env.SMTP_PORT}";
    uiListenAddress = "127.0.0.1:${config.env.SMTP_UI_PORT}";
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
  };

  # This lets Caddy bind to 443
  scripts.caddy-setcap.exec = ''
    sudo setcap 'cap_net_bind_service=+ep' ${pkgs.caddy}/bin/caddy
  '';

  git-hooks.hooks = {
    typos.enable = true;
    markdownlint.enable = true;
    nixfmt-rfc-style.enable = true;
    statix.enable = true;
    deadnix.enable = true;
    prettier.enable = true;
    php-cs-fixer.enable = true;
    phpcs.enable = true;
    psalm.enable = true;
  };
}
