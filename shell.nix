let pkgs = import (builtins.fetchTarball {
      # master on 2021-08-01
      url = "https://github.com/NixOS/nixpkgs/archive/9fc2cddf24ad1819f17174cbae47789294ea6dc4.tar.gz";
      sha256 = "058l6ry119mkg7pwmm7z4rl1721w0zigklskq48xb5lmgig4l332";
    }) { };
    ghcidScript = pkgs.writeShellScriptBin "dev" "ghcid --command 'cabal new-repl lib:matchmaker' --allow-eval --warnings -o ghcid.text";
    formatScript = pkgs.writeShellScriptBin "format" "stylish-haskell -ir ./**/*.hs";
    runScript = pkgs.writeShellScriptBin "run" "cabal run exe:matchmaker";
in with pkgs;
  mkShell {
    shellHook = ''
      source environment.sh
    '';
    buildInputs = [
      # Haskell Deps
      haskell.compiler.ghc8104
      cabal-install
      ghcid
      hlint
      haskellPackages.apply-refact
      stylish-haskell
      git

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

      # Scripts
      ghcidScript
      formatScript
      runScript
    ];
  }
