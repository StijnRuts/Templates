{ pkgs, config, ... }:
let
  DOMAIN = "symfony.localhost";
  DB_HOST = "localhost";
  DB_PORT = 5432;
  DB_NAME = "devdb";
  DB_USER = "devuser";
  DB_PASSWORD = "password";
in
{
  env = {
    inherit DB_HOST;
    inherit DB_PORT;
    inherit DB_NAME;
    inherit DB_USER;
    inherit DB_PASSWORD;
    DATABASE_URL = "postgresql://${DB_USER}:${DB_PASSWORD}@${DB_HOST}:${toString DB_PORT}/${DB_NAME}?serverVersion=16&charset=utf8";
  };

  dotenv.enable = true;

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
    port = DB_PORT;
    initialDatabases = [
      {
        name = DB_NAME;
        user = DB_USER;
        pass = DB_PASSWORD;
        # schema = ./schema.sql;
      }
    ];
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
}
