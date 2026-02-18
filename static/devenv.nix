{ config, ... }:
let
  DOMAIN = "example.localhost";
in
{
  certificates = [ DOMAIN ];

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
      deadnix.enable = true;
      statix.enable = true;
      prettier.enable = true;
    };
  };

  git-hooks.hooks = {
    treefmt.enable = true;
  };
}
