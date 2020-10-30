(import <nixpkgs> {}).haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
    split
])
