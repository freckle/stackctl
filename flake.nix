{
  inputs = {
    stable.url = "github:nixos/nixpkgs/nixos-23.11";
    freckle.url = "git+ssh://git@github.com/freckle/flakes?dir=main";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      nixpkgsArgs = { inherit system; config = { }; };
      nixpkgs = {
        stable = import inputs.stable nixpkgsArgs;
      };
      freckle = inputs.freckle.packages.${system};
      freckleLib = inputs.freckle.lib.${system};
    in
    rec {
      packages = {
        awscli = freckle.aws-cli-2-11-x;

        cabal = nixpkgs.stable.cabal-install;

        fourmolu = freckle.fourmolu-0-13-x;

        ghc = freckleLib.haskellBundle {
          ghcVersion = "ghc-9-6-3";
          packageSelection = p: [ ];
          enableHLS = true;
        };

        hlint =
          nixpkgs.stable.haskell.lib.justStaticExecutables
            nixpkgs.stable.hlint;

        stack = nixpkgs.stable.writeShellApplication {
          name = "stack";
          text = ''
            ${nixpkgs.stable.stack}/bin/stack --system-ghc --no-nix "$@"
          '';
        }
        ;
      };

      devShells.default = nixpkgs.stable.mkShell {
        buildInputs = with (nixpkgs.stable); [
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
}
