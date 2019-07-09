{ withHoogle ? true
}:
(import ./default.nix { inherit withHoogle; }).shell
