{
  description = "tigerbeetle-hs";

  inputs = {
    # Nix Inputs
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    tigerbeetle-src = {
      url = "github:tigerbeetle/tigerbeetle?ref=refs/tags/0.16.33";
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
          pkgs = nixpkgs.legacyPackages.${system};
          hsPkgs = pkgs.haskellPackages.override {
            overrides = hfinal: hprev: {
              tigerbeetle-hs = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "tigerbeetle-hs" ./. {
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
      pkgs,
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
        buildInputs = with pkgs; [
          hsPkgs.haskell-language-server
          haskellPackages.cabal-install
          cabal2nix
          haskellPackages.ghcid
          haskellPackages.fourmolu
          haskellPackages.cabal-fmt
          pkgs.zig
          pkgs.tigerbeetle
          self.packages.${system}.libtb_client
        ];
      });

    # nix build
    packages = forAllSystems ({
      hsPkgs,
      pkgs,
      ...
    }: {
      tigerbeetle-hs = hsPkgs.tigerbeetle-hs;
      libtb_client = pkgs.callPackage ./nix/libtb_client.nix {src = inputs.tigerbeetle-src;};
      default = hsPkgs.tigerbeetle-hs;
      glibc = pkgs.glibc;
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
