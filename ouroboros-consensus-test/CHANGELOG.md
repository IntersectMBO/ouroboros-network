# Changelog for ouroboros-consensus-test

# Changelog entries

<a id='changelog-0.5.0.0'></a>
## 0.5.0.0 — 2023-04-14

### Patch

- The `dev` setting of the `Test.Util.TestEnv` option now leaves the standard `QuickCheckTests` `tasty` option unchanged instead of forcing it to 100, which was causing `--quickcheck-tests` to be ignored.

- `ouroboros-consensus-test`: Since the filesystem API that lives in
  `ouroboros-consensus` will live in the `fs-api` package for now on, start
  depending on `fs-api`, and change imports accordingly.
- `ouroboros-consensus-test`: Since the simulated filesystem that lives in
  `ouroboros-consensus-test` will live in the `fs-sim` package for now on, start
  depending on `fs-sim`, and change imports accordingly.

- Collapse all imports into one group in every file.
- Adapt to relocation of SOP-related `Util` modules.

### Breaking

- `ouroboros-consensus-test`: Move the simulated file system that lives under
  `Test.Util.FS.Sim` to a new package called `fs-sim`. The original modules
  become deprecated.
  
- `mock`, `mock-test` and `tutorials` have become subcomponents of
  `ouroboros-consensus-test`.
