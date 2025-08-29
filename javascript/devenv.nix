{ pkgs, config, ... }:
let
  DOMAIN = "example.localhost";
  BACKEND_PORT = 3000;
in
{
  env = {
    inherit BACKEND_PORT;
  };

  certificates = [ DOMAIN ];

  languages.javascript = {
    enable = true;
    package = pkgs.nodejs-slim_24;
    npm.enable = true;
  };

  scripts = {
    "format:all".exec = "format:backend && format:frontend";
    "lint:all".exec = "lint:backend && lint:frontend";
  };

  services.caddy = {
    enable = true;
    virtualHosts.${DOMAIN} = {
      extraConfig = ''
        tls ${config.env.DEVENV_STATE}/mkcert/${DOMAIN}.pem ${config.env.DEVENV_STATE}/mkcert/${DOMAIN}-key.pem
        root * public
        file_server
        reverse_proxy /api/* localhost:${toString BACKEND_PORT}
      '';
    };
  };

  # This lets Caddy bind to privileged ports like 80 and 443
  scripts.caddy-setcap.exec = ''
    sudo setcap 'cap_net_bind_service=+ep' ${pkgs.caddy}/bin/caddy
  '';
}
