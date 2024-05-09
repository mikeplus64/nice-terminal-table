{
  description = "char-boxdrawing";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:gytis-ivaskevicius/flake-utils-plus";
    flake-root.url = "github:srid/flake-root";
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ flake-utils.overlay ];
      };

      packageName = "char-boxdrawing";
      ghc = "ghc946";

      hlib = pkgs.haskell.lib;
      jailbreakUnbreak = pkg: hlib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));
      dontCheck = hlib.dontCheck;
      appendPatch = hlib.appendPatch;

      hp = pkgs.haskell.packages.${ghc}.override {
        overrides = hp: super: {
          ${packageName} = hp.callCabal2nix packageName self {};
        };
      };

    in flake-utils.lib.eachSystem [ system ] (system: {
      packages.default = hp.${packageName};
      defaultPackage = self.packages.${system}.default;
      devShells.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          hp.haskell-language-server
          ghcid
          cabal-install
          hlint
          hp.fourmolu
          hp.cabal-fmt
          cabal2nix
          treefmt
          just
          fup-repl
        ];
        inputsFrom = map (__getAttr "env") (__attrValues self.packages.${system});
      };
      devShell = self.devShells.${system}.default;
    });
}
