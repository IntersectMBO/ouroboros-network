{ pkgs, lib }:

rec {

  flattenDerivationTree = separator: set:
    let
      recurse = name: name':
        flatten (if name == "" then name' else "${name}${separator}${name'}");

      flatten = name': value:
        let 
          name = builtins.replaceStrings [":"] [separator] name';
        in 
          if lib.isDerivation value || lib.typeOf value != "set" then
            [{ inherit name value; }]
          else
            lib.concatLists (lib.mapAttrsToList (recurse name) value);
    in
      assert lib.typeOf set == "set"; 
      lib.listToAttrs (flatten "" set);


  mapAttrsValues = f: lib.mapAttrs (_name: f);


  makeHydraRequiredJob = hydraJobs:
    let
      cleanJobs = lib.filterAttrsRecursive 
        (name: _: name != "recurseForDerivations")
        (removeAttrs hydraJobs [ "required" ]);
    in
      pkgs.releaseTools.aggregate {
        name = "required";
        meta.description = "All jobs required to pass CI";
        constituents = lib.collect lib.isDerivation cleanJobs;
      };
}
