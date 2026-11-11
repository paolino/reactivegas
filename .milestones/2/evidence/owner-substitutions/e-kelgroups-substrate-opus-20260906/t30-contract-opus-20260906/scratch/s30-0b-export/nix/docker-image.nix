{ pkgs, project, version, clientDist }:

pkgs.dockerTools.buildImage {
  name = "ghcr.io/paolino/kelgroups";
  tag = version;
  config = {
    EntryPoint =
      [ "kelgroups-server" "3001" "/data/kelgroups.db" "bootstrap" ];
    ExposedPorts = { "3001/tcp" = { }; };
    Volumes = { "/data" = { }; };
    WorkingDir = "/app";
  };
  copyToRoot = pkgs.buildEnv {
    name = "image-root";
    paths = [
      project.packages."kelgroups:exe:kelgroups-server"
      (pkgs.runCommand "client-files" { } ''
        mkdir -p $out/app/client/kelgroups-trivial/dist
        cp ${clientDist}/index.js \
          $out/app/client/kelgroups-trivial/dist/
        cp ${clientDist}/index.html \
          $out/app/client/kelgroups-trivial/dist/
        cp ${clientDist}/style.css \
          $out/app/client/kelgroups-trivial/dist/
      '')
    ];
  };
}
