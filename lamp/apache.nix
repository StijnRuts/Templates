{
  pkgs,
  lib,
  config,
  ...
}:

let
  cfg = config.services.apache;
  configFile = pkgs.writeText "httpd.conf" ''
    ServerRoot "${config.env.DEVENV_STATE}/apache"
    PidFile "${config.env.DEVENV_STATE}/apache/httpd.pid"
    ErrorLog "/dev/stderr"
    LogLevel debug

    LoadModule authz_core_module ${cfg.package}/modules/mod_authz_core.so
    LoadModule dir_module ${cfg.package}/modules/mod_dir.so
    LoadModule mpm_event_module ${cfg.package}/modules/mod_mpm_event.so
    LoadModule proxy_module ${cfg.package}/modules/mod_proxy.so
    LoadModule proxy_fcgi_module ${cfg.package}/modules/mod_proxy_fcgi.so
    LoadModule rewrite_module ${cfg.package}/modules/mod_rewrite.so
    LoadModule ssl_module ${cfg.package}/modules/mod_ssl.so
    LoadModule unixd_module ${cfg.package}/modules/mod_unixd.so

    ServerName ${cfg.hostname}

    Listen 80
    <VirtualHost *:80>
      ServerName ${cfg.hostname}
      ServerAlias www.${cfg.hostname}

      DocumentRoot "${config.env.DEVENV_ROOT}/public"
      <Directory "${config.env.DEVENV_ROOT}/public">
        Options Indexes FollowSymLinks
        AllowOverride All
        Require all granted
        DirectoryIndex index.php index.html
      </Directory>

      <FilesMatch \.php$>
        SetHandler "proxy:unix:${cfg.phpSocket}|fcgi://localhost/"
      </FilesMatch>
    </VirtualHost>

    Listen 443
    <VirtualHost *:443>
      ServerName ${cfg.hostname}
      ServerAlias www.${cfg.hostname}

      SSLEngine on
      SSLCertificateFile "${cfg.sslCert}"
      SSLCertificateKeyFile "${cfg.sslKey}"

      DocumentRoot "${config.env.DEVENV_ROOT}/public"
      <Directory "${config.env.DEVENV_ROOT}/public">
        Options Indexes FollowSymLinks
        AllowOverride All
        Require all granted
        DirectoryIndex index.php index.html
      </Directory>

      <FilesMatch \.php$>
        SetHandler "proxy:unix:${cfg.phpSocket}|fcgi://localhost/"
      </FilesMatch>
    </VirtualHost>

    ${cfg.extraConfig}
  '';
in
{
  options.services.apache = {
    enable = lib.mkEnableOption "Apache HTTP Server";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.apacheHttpd;
      defaultText = lib.literalExpression "pkgs.apacheHttpd";
      description = "The Apache HTTP Server package to use.";
    };

    hostname = lib.mkOption {
      type = lib.types.str;
      description = "The domain name of the website.";
    };

    sslCert = lib.mkOption {
      type = lib.types.path;
      description = "Path to the SSL certificate file.";
    };

    sslKey = lib.mkOption {
      type = lib.types.path;
      description = "Path to the SSL private key file.";
    };

    phpSocket = lib.mkOption {
      type = lib.types.str;
      description = "The PHP-FPM socket to use.";
    };

    extraConfig = lib.mkOption {
      type = lib.types.lines;
      default = "";
      description = "Additional Apache configuration directives.";
    };

    configFile = lib.mkOption {
      type = lib.types.path;
      default = configFile;
      internal = true;
      description = "The Apache configuration file.";
    };
  };

  config = lib.mkIf cfg.enable {
    processes.apache.exec = "${cfg.package}/bin/httpd -f ${cfg.configFile} -DFOREGROUND";

    enterShell = ''
      mkdir -p ${config.env.DEVENV_STATE}/apache
    '';
  };
}
