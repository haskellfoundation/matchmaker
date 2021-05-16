let pkgs = import (fetchTarball path) {};
    path = https://github.com/NixOS/nixpkgs/archive/master.tar.gz;
in with pkgs;
  mkShell {
    buildInputs = [
      # Haskell Deps
      haskell.compiler.ghc8104
      cabal-install
      ghcid
      hlint
      haskellPackages.apply-refact
      stylish-haskell

      # Frontend Deps
      yarn
      nodejs-14_x

      # DB Deps
      postgresql_12
      gmp
      zlib
      glibcLocales

      # Extra
      direnv
      parallel
    ];
  }
