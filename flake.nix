{
  description = "tigerbeetle-hs";

  inputs = {
    # Nix Inputs
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    tigerbeetle-src = {
      url = "github:tigerbeetle/tigerbeetle?ref=refs/tags/0.16.67";
      flake = false;
    };
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    tigerbeetle-src,
  }: let
    forAllSystems = function:
      nixpkgs.lib.genAttrs [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ] (system:
        function rec {
          inherit system;
          compilerVersion = "ghc984";
          legacyPkgs = nixpkgs.legacyPackages.${system};
          hsPkgs = legacyPkgs.haskellPackages.override {
            overrides = hfinal: hprev: {
              tigerbeetle-hs = legacyPkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "tigerbeetle-hs" ./. {
                tb_client = self.packages.${system}.libtb_client;
              });
            };
          };
        });
  in {
    # nix fmt
    formatter = forAllSystems ({pkgs, ...}: pkgs.alejandra);

    # nix develop
    devShell = forAllSystems ({
      hsPkgs,
      legacyPkgs,
      system,
      ...
    }:
      hsPkgs.shellFor {
        # withHoogle = true;
        shellHook = ''
          canonical="${tigerbeetle-src}/src/clients/c/tb_client.h"
          local="./include/tb_client.h"
          cmp --silent $canonical $local || cat $canonical > $local
        '';
        packages = p: [
          p.tigerbeetle-hs
        ];
        buildInputs = with legacyPkgs; [
          hsPkgs.haskell-language-server
          haskellPackages.cabal-install
          cabal2nix
          haskellPackages.ghcid
          haskellPackages.fourmolu
          haskellPackages.cabal-fmt
          haskellPackages.weeder
          legacyPkgs.zig_0_14
          self.packages.${system}.libtb_client
          self.packages.${system}.tigerbeetle
        ];
      });

    # nix build
    packages = forAllSystems ({
      hsPkgs,
      legacyPkgs,
      ...
    }: {
      tigerbeetle-hs = hsPkgs.tigerbeetle-hs;
      libtb_client = legacyPkgs.callPackage ./nix/libtb_client.nix {src = inputs.tigerbeetle-src;};
      tigerbeetle = legacyPkgs.callPackage ./nix/tigerbeetle.nix {src = inputs.tigerbeetle-src;};
      default = hsPkgs.tigerbeetle-hs;
      glibc = legacyPkgs.glibc;
    });

    # You can't build the tigerbeetle-hs package as a check because of IFD in cabal2nix
    checks = {};

    # nix run
    apps = forAllSystems ({system, ...}: {
      tigerbeetle-hs = {
        type = "app";
        program = "${self.packages.${system}.tigerbeetle-hs}/bin/tigerbeetle-hs";
      };
      default = self.apps.${system}.tigerbeetle-hs;
    });
  };
}
