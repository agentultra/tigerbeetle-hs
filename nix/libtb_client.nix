{
  lib,
  stdenv,
  zig,
  src,
  autoPatchelfHook,
  fixDarwinDylibNames,
}:
let
  # '-Dcpu=baseline' causes a build failure; realistically this should use some
  # sort of cross-compilation arch selection process.
  zig-hook = zig.hook.overrideAttrs {
    zig_default_flags = ["--release=safe"];
  };
  # FIXME: We should be able to more automatically map between Nix & Zig
  # architectures.
  arch-map = {
    "x86_64-linux" = "x86_64-linux-gnu.2.27";
    "aarch64-linux" = "aarch64-linux-gnu.2.27";
    "x86_64-darwin" = "x86_64-macos";
    "aarch64-darwin" = "aarch64-macos";
  };
in stdenv.mkDerivation {
  pname = "libtb_client";
  version = builtins.substring 0 7 src.rev;
  inherit src;
  nativeBuildInputs = [
    zig-hook
  ] ++ lib.optionals stdenv.hostPlatform.isDarwin [
    fixDarwinDylibNames
  ] ++ lib.optionals stdenv.hostPlatform.isLinux [
    autoPatchelfHook
  ];

  patches = lib.optionals stdenv.hostPlatform.isDarwin [
    ./darwin-headerpad-max-install-names.patch
  ];

  dontUseZigInstall = true;
  dontConfigure = true;

  zigBuildFlags = ["-Dgit-commit=${src.rev}" "--color off" "clients:c"];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/lib
    install -m555 ./src/clients/c/lib/${builtins.getAttr stdenv.hostPlatform.system arch-map}/* $out/lib

    runHook postInstall
  '';

  # FIXME: This needs documentation lol.
  preFixup = lib.optional stdenv.hostPlatform.isLinux ''
    patchelf --add-needed libm.so.6 $out/lib/libtb_client.so
  '';

  meta = {
    platforms = builtins.attrNames arch-map;
    pkgConfigModules = "tb_client";
  };
}
