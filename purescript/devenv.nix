{
  pkgs,
  easy-purescript-nix,
  config,
  ...
}:
let
  DOMAIN = "example.localhost";
  easy-ps = easy-purescript-nix.packages.x86_64-linux;
in
{
  certificates = [ DOMAIN ];

  packages = [
    easy-ps.purs-0_15_9
    easy-ps.purescript-language-server
    easy-ps.spago
    easy-ps.purs-tidy
    easy-ps.psa
    easy-ps.pscid
    easy-ps.purs-backend-es
    pkgs.esbuild
  ];

  languages.purescript.enable = true;

  languages.javascript = {
    enable = true;
    package = pkgs.nodejs-slim_24;
    npm.enable = true;
  };

  scripts = {
    run.exec = "spago run";
    build.exec = "spago build";
    bundle.exec = "spago bundle-app --to public/index.js --source-maps";
    watch.exec = "spago bundle-app --to public/index.js --source-maps --watch";
    prod.exec = ''
      spago build --config spago-prod.dhall \
        && purs-backend-es bundle-app --no-build --to public/index.js --minify
    '';
    format.exec = "purs-tidy format-in-place 'src/**/*.purs'";
    repl.exec = "spago repl";
    interactive.exec = "pscid";
    docs.exec = "spago docs --format html --open";
    test.exec = "spago test --config spago-test.dhall";
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
    purs-tidy = {
      enable = true;
      name = "purs-tidy";
      entry = "purs-tidy format-in-place";
      files = "\\.purs$";
    };
  };
}
