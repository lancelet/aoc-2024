{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    utils,
  }:
    utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {inherit system;};
        hpkgs = pkgs.haskell.packages."ghc966";
        stack-wrapped = pkgs.symlinkJoin {
          name = "stack";
          paths = [pkgs.stack];
          buildInputs = [pkgs.makeWrapper];
          postBuild = ''
            wrapProgram $out/bin/stack \
              --add-flags "\
                --no-nix \
                --system-ghc \
                --no-install-ghc \
              "
          '';
        };
      in {
        devShell = with pkgs;
          mkShell {
            buildInputs = [
              hpkgs.ghc
              stack-wrapped
              hpkgs.hlint
              hpkgs.hoogle
              hpkgs.haskell-language-server
              hpkgs.implicit-hie
              hpkgs.retrie
	      hpkgs.zlib
              nodejs
	      zlib
            ];
          };
      }
    );
}
