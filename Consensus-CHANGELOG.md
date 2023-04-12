# Consensus Changelog

The Consensus packages are organized in four bundles: the packages that are
ledger-agnostic, the packages that are Cardano specific, and their testing
bundles respectively. For each of these bundles we keep a separate
`CHANGELOG.md` file, and the version number of all the packages in each bundle
moves in lockstep. We use symlinks to unify the `changelog.d` directory in each
of the bundles.

The two Changelogs can be found here:
- [ledger-agnostic bundle](./ouroboros-consensus/CHANGELOG.md)
- [ledger-agnostic testing bundle](./ouroboros-consensus-test/CHANGELOG.md)
- [Cardano-specific bundle](./ouroboros-consensus-cardano/CHANGELOG.md)
- [Cardano-specific testing bundle](./ouroboros-consensus-cardano-test/CHANGELOG.md)

If you have any doubts, please consult the [release
process](./ouroboros-consensus/docs/ReleaseProcess.md).

In short:
- To create a new changelog entry, navigate to the root of the package you want
  to create an entry for and run `scriv create`. This will create a template
  file in `./changelog.d` which you have to edit with the information of your
  entry.
- When doing a release, `scriv collect` collects all the pending changelog
  entries and put them in the `CHANGELOG.md` file.
