# Consensus Changelog

The Consensus packages are organized in two bundles: the packages that are
ledger-agnostic and the packages that are Cardano specific. For each of these
bundles we keep a separate `CHANGELOG.md` file, and the version number of all
the packages in each bundle moves in lockstep. We use symlinks to unify the
`changelog.d` directory in each of the bundles.

The two Changelogs can be found here:
- [ledger-agnostic bundle](./ouroboros-consensus/CHANGELOG.md)
- [Cardano-specific bundle](./ouroboros-consensus-cardano/CHANGELOG.md)

If you have any doubts, please consult the [release
process](./ouroboros-consensus/docs/ReleaseProcess.md).

In short:
- To create a new changelog entry, navigate to the root of the package you want
  to create an entry for and run `scriv create`. This will create a template
  file in `./changelog.d` which you have to edit with the information of your
  entry.
- When doing a release, `scriv collect` collects all the pending changelog
  entries and put them in the `CHANGELOG.md` file.
