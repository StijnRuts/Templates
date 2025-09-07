{
  pkgs,
  config,
  ...
}:
let
  DOMAIN = "example.localhost";
in
{
  certificates = [ DOMAIN ];

  languages.typescript.enable = true;

  languages.javascript = {
    enable = true;
    package = pkgs.nodejs-slim_24;
    npm.enable = true;
  };

  scripts = {
    "build".exec = "npm run build";
    "build:watch".exec = "npm run build:watch";
    "format".exec = "npm run format";
    "lint".exec = "npm run lint";
  };

  services.caddy = {
    enable = true;
    virtualHosts.${DOMAIN} = {
      extraConfig = ''
        tls ${config.env.DEVENV_STATE}/mkcert/${DOMAIN}.pem ${config.env.DEVENV_STATE}/mkcert/${DOMAIN}-key.pem
        root * public
        file_server
      '';
    };
  };

  # This lets Caddy bind to privileged ports like 80 and 443
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
    eslint.enable = true;
  };
}
