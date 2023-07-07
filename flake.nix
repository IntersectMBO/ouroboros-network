{
  inputs = {
    nixpkgs.follows = "std/nixpkgs";
    std.url = "github:divnix/std";
  };

  outputs = { self, std, ... }@inputs:
    std.growOn {
      inherit inputs;
      cellsFrom = nix/cells;
      cellBlocks = [
        (std.functions "library")
        (std.functions "hydraJobs")
        (std.functions "actions")
      ];
    } { hydraJobs = std.harvest self [ "automation" "hydraJobs" ]; };

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys =
      [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
    allow-import-from-derivation = true;
  };
}
