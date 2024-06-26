{
  description = "nice-terminal-table";

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

      packageName = "nice-terminal-table";
      ghc = "ghc946";

      hlib = pkgs.haskell.lib;
      jailbreakUnbreak = pkg: hlib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));
      dontCheck = hlib.dontCheck;
      appendPatch = hlib.appendPatch;

      hp = pkgs.haskell.packages.${ghc}.override {
        overrides = hp: super: {
          nice-terminal-table = hp.callCabal2nix "nice-terminal-table" self {};
          char-boxdrawing = hp.callCabal2nix "char-boxdrawing" ./char-boxdrawing {};
          kittyprint = hp.callCabal2nix "kittyprint" ./kittyprint {};
        };
      };

    in flake-utils.lib.eachSystem [ system ] (system: {
      packages.default = hp.${packageName};
      defaultPackage = self.packages.${system}.default;
      devShell = hp.shellFor {
        packages = hp: [
          hp.nice-terminal-table
          hp.char-boxdrawing
          hp.kittyprint
        ];
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
        ];
      };
    });
}
