The IOG Consensus team uses this `cabal-lint` tool to ensures some invariants among its `.cabal` files.

The invariants are one-to-one with the possible constructors of values in the `Cabal.Lint.UX.Output.Message` type that `messageSeverity` maps to `ErrorSeverity`.
Currently, those are:

- Component-level errors
    - `RepeatDeps` - a component's `build-depends` fields list the same library multiple times
    - `RepeatOptions` - a component's `ghc-options` fields list the same option multiple times
    - `MissingOptions` - a component's `ghc-options` fields do not necessarily list every option we require (see `Cabal.Lint.Main.requiredOptions`)
    - `NonEmptyDefaultExtensions` - a component's `default-extensions` field isn't empty
- Package-level errors
    - `NonEmptyForeignLibraries` - a package declares a `foreign-library` component
    - `CouldNotParse` - the `.cabal` file could not be parsed
- Project-level errors (the _project_ is all `.cabal` file paths passed to the tool at once)
    - `EmptyProject` - there are no `.cabal` files listed on the command-line
    - `InconsistentVersionRanges` - the `build-depends` fields of components declared in the given `.cabal` files constrain the same library to different version ranges
    - `InconsistentDefaultLanguages` - the `default-language` fields of components declared in the given `.cabal` files list different values

Conditionals in the `.cabal` files are handled in a naive-but-reasonable way: the tool requires that all invariants are definitely respected regardless of the conditions' values, assuming only that the two branches of any one conditional are exclusive.
