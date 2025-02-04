{
  pkgs,
  tigerbeetle-src,
  system ? "x86_64-linux",
}: let
  zig_hook = pkgs.zig.hook;

  zigCache = pkgs.stdenv.mkDerivation {
    name = "tigerbeetle-cache";
    src = tigerbeetle-src;
    nativeBuildInputs = [
      pkgs.git
      zig_hook
    ];

    dontConfigure = true;
    dontUseZigBuild = true;
    dontUseZigInstall = true;
    dontFixup = true;

    buildPhase = ''
      runHook preBuild

      zig build --fetch

      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall

      cp -r --reflink=auto $ZIG_GLOBAL_CACHE_DIR $out

      runHook postInstall
    '';

    outputHashMode = "recursive";
    outputHash = "sha256-pQpattmS9VmO3ZIQUFn66az8GSmB4IvYhTTCFn6SUmo=";
  };
  allowedSystems = [
    "x86_64-linux"
    "aarch64-linux"
    "x86_64-darwin"
    "aarch64-darwin"
  ];
  systemToTBDir = {
    "x86_64-linux" = "x86_64-linux-musl";
    "aarch64-linux" = "aarch64-linux-musl";
    "x86_64-darwin" = "x86_64-macos";
    "aarch64-darwin" = "aarch64-macos";
  };
in
  if pkgs.lib.lists.any (sys: sys == system) allowedSystems
  then
    pkgs.stdenv.mkDerivation (finalAttrs: {
      name = "libtb_client";
      version = tigerbeetle-src.rev;
      src = pkgs.fetchFromGitHub {
        leaveDotGit = true;
        owner = "tigerbeetle";
        repo = "tigerbeetle";
        rev = tigerbeetle-src.rev;
        hash = "sha256-V8g1bvqINo8WxFSv8AkqE/WPUcPwQ1BtisV9+tLyEp4=";
      };
      nativeBuildInputs = [
        pkgs.git
        zig_hook
      ];
      zigBuildFlags = "-Dversion-string=${finalAttrs.version}-nix";
      dontConfigure = true;
      preBuild = ''
        rm -rf $ZIG_GLOBAL_CACHE_DIR
        cp -r --reflink=auto ${zigCache} $ZIG_GLOBAL_CACHE_DIR
        chmod u+rwX -R $ZIG_GLOBAL_CACHE_DIR
      '';
      buildPhase = ''
        zig build clients:c
      '';
      installPhase = ''
        mkdir -p $out/lib
        cp -r ./src/clients/c/lib/${systemToTBDir.${system}}/* $out/lib
      '';
      meta.pkgConfigModules = "tb_client";
    })
  else throw "Right now libtb_client only supports these operating systems ${allowedSystems}"
