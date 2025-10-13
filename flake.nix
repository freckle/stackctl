{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    freckle.url = "github:freckle/flakes?dir=main";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      nixpkgs = inputs.nixpkgs.legacyPackages.${system};
      freckle = inputs.freckle.packages.${system};
      freckleLib = inputs.freckle.lib.${system};
    in
    rec {
      packages = {
        awscli = freckle.aws-cli-2-11-x;

        cabal = nixpkgs.cabal-install;

        fourmolu = freckle.fourmolu-0-13-x;

        ghc = freckleLib.haskellBundle {
          ghcVersion = "ghc-9-8-4";
          packageSelection = p: [ ];
          enableHLS = true;
        };

        hlint =
          nixpkgs.haskell.lib.justStaticExecutables
            nixpkgs.hlint;

        stack = nixpkgs.writeShellApplication {
          name = "stack";
          text = ''
            ${nixpkgs.stack}/bin/stack --system-ghc --no-nix "$@"
          '';
        }
        ;
      };

      devShells.default = nixpkgs.mkShell {
        buildInputs = with (nixpkgs); [
          pcre
          pcre.dev
          zlib
          zlib.dev
        ];

        nativeBuildInputs = with (packages); [
          awscli
          cabal
          fourmolu
          ghc
          hlint
          stack
        ];

        shellHook = ''
          export STACK_YAML=stack.yaml
        '';
      };
    });

  nixConfig = {
    extra-substituters = [
      "https://freckle.cachix.org"
      "https://freckle-private.cachix.org"
    ];
    extra-trusted-public-keys = [
      "freckle.cachix.org-1:WnI1pZdwLf2vnP9Fx7OGbVSREqqi4HM2OhNjYmZ7odo="
      "freckle-private.cachix.org-1:zbTfpeeq5YBCPOjheu0gLyVPVeM6K2dc1e8ei8fE0AI="
    ];
  };
}
