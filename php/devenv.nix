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
  dotenv.enable = true;

  packages = with pkgs; [
    php83Packages.php-cs-fixer
    php84Packages.psalm
    php84Packages.deployer
  ];

  scripts = {
    "style:check".exec = "php-cs-fixer check src";
    "style:fix".exec = "php-cs-fixer fix src";
    "code:check".exec = "psalm --show-info=true";
    "code:fix".exec = "psalm --alter --issues=all";
    "test".exec = "./vendor/bin/phpunit tests";
    "deploy".exec = "dep deploy";
  };

  certificates = [ DOMAIN ];

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
    listen_addresses = "localhost";
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
