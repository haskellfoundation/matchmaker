let
  pkgs =
    let
      # master on 2021-08-01
      rev = "9fc2cddf24ad1819f17174cbae47789294ea6dc4";
      sha256 = "058l6ry119mkg7pwmm7z4rl1721w0zigklskq48xb5lmgig4l332";
      nixpkgs = builtins.fetchTarball {
        url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
        inherit sha256;
      };
    in
    import nixpkgs {
      config = { };
      overlays = [ haskellPackagesOverlay ];
    };

  haskellPackagesOverlay = pkgsFinal: pkgsPrev:
    let
      extension = haskellPackagesFinal: haskellPackagesPrev: {
        "matchmaker" =
          pkgsFinal.haskell.lib.overrideCabal
            (haskellPackagesFinal.callCabal2nix "matchmaker" ./. { })
            (old: {
              doCheck = false;
            });

        "hspec-pg-transact" =
          haskellPackagesFinal.callCabal2nix
            "hspec-pg-transact"
            (pkgsFinal.fetchFromGitHub {
              owner = "jfischoff";
              repo = "pg-transact-hspec";
              rev = "77e6a97e60838fcf5763310204b0a74835e4f1d5";
              sha256 = "1j6a0zlw0hmp7avgp5af65kwzc2kngkrsq7dwiz1pdn3mzrvlpzk";
            })
            { };

        "pg-entity" =
          pkgsFinal.haskell.lib.overrideCabal
            (haskellPackagesFinal.callCabal2nix
              "pg-entity"
              (pkgsFinal.fetchFromGitHub {
                owner = "tchoutri";
                repo = "pg-entity";
                rev = "c45277e8a8525737e09de05c5a3b012e5093887a";
                sha256 = "0lpb1hqv1gccwaz17pih58b8k1vpxxy08k12sxgaxz6q8miav41i";
              })
              { })
            (old: {
              doCheck = false;
            });

        "pg-transact" =
          pkgsFinal.haskell.lib.overrideCabal
            haskellPackagesPrev.pg-transact
            (old: {
              broken = false;
              doCheck = false;
            });
      };
    in
    {
      haskellPackages = pkgsPrev.haskellPackages.override (old: {
        overrides =
          pkgsPrev.lib.fold
            pkgsPrev.lib.composeExtensions
            (old.overrides or (_: _: { }))
            [
              extension
            ];
      });
    };

  ghcidScript =
    pkgs.writeShellScriptBin "dev" ''
      ghcid --command 'cabal new-repl lib:matchmaker' --allow-eval --warnings -o ghcid.text
    '';

  formatScript =
    pkgs.writeShellScriptBin "format" ''
      stylish-haskell -ir ./**/*.hs
    '';

  runScript = pkgs.writeShellScriptBin "run" ''
    cabal run exe:matchmaker
  '';

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
