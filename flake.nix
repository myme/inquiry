{
  description = "A Haskell restclient.el clone";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, utils }:
    let name = "restclient";
    in {
      overlay = (final: prev: {
        haskellPackages = prev.haskellPackages // {
          ${name} = prev.haskellPackages.callCabal2nix name ./. {};
        };
      });
    } // utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
        };
      in {
        defaultPackage = pkgs.${name};

        devShell = pkgs.callPackage ./shell.nix {
          inherit name;
          package = pkgs.haskellPackages.${name};
        };
      });
}
