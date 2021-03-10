{ sources ? import ./nix/sources.nix { inherit system; }
, pkgs ? import sources.nixpkgs { inherit system; }
, compiler ? "ghc8103"
, system ? builtins.currentSystem
}:
let
  overrides = foldOs [
    ((import sources.sawtooth-haskell-protos { nixpkgs = pkgs; ghc = compiler; inherit system; }).sawtooth-haskell-protos-overlay)
    (hself: hsuper: {
      type-level-sets = hsuper.callCabal2nix "type-level-sets" sources.type-level-sets { };
      generic-arbitrary = hsuper.callCabal2nix "generic-arbitrary" sources.generic-arbitrary { };
      secp256k1-haskell = hsuper.callCabal2nix "secp256k1-haskell" sources.secp256k1-haskell { inherit (pkgs) secp256k1; };
      sawtooth-sdk = hsuper.callCabal2nix "sawtooth-sdk" ./. { };
    })
  ];
  haskell = pkgs.haskell.packages.${compiler}.override {
    inherit overrides;
  };
  foldOs = pkgs.lib.foldl' pkgs.lib.composeExtensions (self: super: { });

in
haskell.sawtooth-sdk
