{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", withProfiling ? false, withHoogle ? true }:

let

  inherit (nixpkgs) pkgs;
  lib = import "${nixpkgs.path}/pkgs/development/haskell-modules/lib.nix" { pkgs = nixpkgs; };

  haskellPackagesWithCompiler = 
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  haskellPackagesWithProfiling = 
    if withProfiling
    then haskellPackagesWithCompiler.override {
           overrides = self: super: {
             mkDerivation = args: super.mkDerivation (args // { enableLibraryProfiling = true; });
           };
         }
    else haskellPackagesWithCompiler;
                 
  haskellPackagesWithHoogle =
    if withHoogle
    then haskellPackagesWithProfiling.override {
           overrides = self: super: {
             ghc = super.ghc // { withPackages = super.ghc.withHoogle; };
             ghcWithPackages = self.ghc.withPackages;
           };
         }
    else haskellPackagesWithProfiling;

  cabal2nixResult = src: nixpkgs.runCommand "cabal2nixResult" {
    buildCommand = ''
      cabal2nix file://"${src}" >"$out"
    '';
    buildInputs = with nixpkgs; [
      cabal2nix
    ];
  } "";

  sources = {
    # This is where to put the output from nix-prefetch-git
    #
    # This is based on the results o
    #   nix-prefetch-git http://github.com/ekmett/mtl
    #
    # For general git fetching:
    #
    # mtl = fetchgit {
    #   url = "http://github.com/ekmett/mtl";
    #   rev = "f75228f7a750a74f2ffd75bfbf7239d1525a87fe";
    #   sha256= "032s8g8j4djx7y3f8ryfmg6rwsmxhzxha2qh1fj15hr8wksvz42a";
    # };
    #
    # Or, more efficient for github repos:
    #
    # mtl = fetchFromGitHub {
    #   owner = "ekmett";
    #   repo = "mtl";
    #   rev = "f75228f7a750a74f2ffd75bfbf7239d1525a87fe";
    #   sha256= "032s8g8j4djx7y3f8ryfmg6rwsmxhzxha2qh1fj15hr8wksvz42a";
    # };
    bound = pkgs.fetchFromGitHub {
      owner = "ekmett";
      repo = "bound";
      rev = "179212e348279216ed221a8ec85b0d1caa92c9c1";
      sha256 = "0cwwprf9d88xig6p5v1sfbs8mwbi00340f7n01qj61rklz3iywy3";
    };
  };

  modifiedHaskellPackages = haskellPackagesWithHoogle.override {
    overrides = self: super: {
      # Add various dependencies here.
      #
      # Local dependencies:
      # my-dependency = self.callPackage ./deps/my-dependency {};
      #
      # Local dependencies with tests disabled:
      # my-dependency = lib.dontCheck (self.callPackage ./deps/my-dependency {});
      #
      # Git dependencies:
      # mtl = self.callPackage (cabal2nixResult sources.mtl) {};

      bound = self.callPackage (cabal2nixResult sources.bound // { version = "1.0.8"; }) {};
    };
  }; 

  drv = modifiedHaskellPackages.callPackage ./. {};

in

  if pkgs.lib.inNixShell then drv.env else drv
