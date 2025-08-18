{
  inputs,
  pkgs,
  lib,
  config,
  ...
}:
{
  # This creates a local copy of Caddy that can bind to privileged ports like 80 and 443
  scripts.caddy-setcap.exec = lib.mkForce ''
    mkdir -p ./bin
    cp ${pkgs.caddy}/bin/caddy ./bin/caddy
    sudo setcap 'cap_net_bind_service=+ep' ./bin/caddy
  '';

  # This launces the local Caddy
  processes.caddy-local.exec = ''${lib.replaceString "${pkgs.caddy}" "." config.processes.caddy.exec}'';
}
