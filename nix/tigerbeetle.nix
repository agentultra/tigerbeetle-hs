{
  lib,
  stdenv,
  zig_0_14,
  src,
  pkg-config,
  autoPatchelfHook,
  fixDarwinDylibNames,
}:
let
  # '-Dcpu=baseline' causes a build failure; realistically this should use some
  # sort of cross-compilation arch selection process.
  zig = zig_0_14;
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
  pname = "tigerbeetle";
  version = builtins.substring 0 7 src.rev;
  inherit src;
  nativeBuildInputs = [
    zig-hook
  ];

  dontUseZigInstall = true;
  dontConfigure = true;

  zigBuildFlags = ["-Dgit-commit=${src.rev}" "--color off"];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    install -m555 ./tigerbeetle $out/bin/tigerbeetle

    runHook postInstall
  '';

  meta = {
    platforms = builtins.attrNames arch-map;
  };
}
