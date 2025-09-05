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
  dotenv.enable = true;

  certificates = [
    DOMAIN
    ("www." + DOMAIN)
  ];

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

  services.apache = {
    enable = true;
    hostname = DOMAIN;
    sslCert = "${config.env.DEVENV_STATE}/mkcert/${DOMAIN}+1.pem";
    sslKey = "${config.env.DEVENV_STATE}/mkcert/${DOMAIN}+1-key.pem";
    phpSocket = "${config.languages.php.fpm.pools.web.socket}";
  };

  # This lets Apache bind to 80 and 443
  scripts.apache-setcap.exec = ''
    sudo setcap 'cap_net_bind_service=+ep' ${pkgs.apacheHttpd}/bin/httpd
  '';
}
