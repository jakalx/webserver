{
  description = "Webserver defined in sockets-and-pipes book";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";

    flake-utils.url = "github:numtide/flake-utils";
    flake-utils.inputs.nixpkgs.follows = "nixpkgs";

    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, pre-commit-hooks }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskell.packages.ghc92;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "webserver-pns";
      in {
        packages.${packageName} = haskellPackages.callCabal2nix packageName self
          rec {
            # Dependency overrides go here
          };

        defaultPackage = self.packages.${system}.${packageName};

        apps = {
          # run with: nix run #.hello
          hello = {
            type = "app";
            program = "${self.defaultPackage.${system}}/bin/hello";
          };

          # run with: nix run
          default = self.apps.${system}.hello;
        };

        checks = {
          pre-commit-check = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              nixfmt.enable = true;
              fourmolu.enable = true;
              hpack.enable = true;
              hlint.enable = true;
            };
          };
        };

        devShell = haskellPackages.shellFor {
          inherit (self.checks.${system}.pre-commit-check) shellHook;

          packages = p: [ self.defaultPackage.${system} ];

          withHoogle = true;

          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server
            haskellPackages.fourmolu
            cabal-install
            ghcid
            nixfmt
            hpack
            hlint
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
