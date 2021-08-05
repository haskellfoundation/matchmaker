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
  overlays = [
    (import ./overlays/haskell-packages.nix)
  ];
}
