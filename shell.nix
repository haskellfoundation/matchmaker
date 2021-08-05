let pkgs = import ./nix/pkgs.nix;
    ghcidScript = pkgs.writeShellScriptBin "dev" "ghcid --command 'cabal new-repl lib:matchmaker' --allow-eval --warnings -o ghcid.text";
    formatScript = pkgs.writeShellScriptBin "format" "stylish-haskell -ir ./**/*.hs";
    runScript = pkgs.writeShellScriptBin "run" "cabal run exe:matchmaker";
in
  pkgs.haskellPackages.matchmaker.env.overrideAttrs (old: {
    shellHook = ''
      ${old.shellHook or ""}
      source environment.sh
    '';
    buildInputs = with pkgs; (old.buildInputs or [ ]) ++ [
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
  })
