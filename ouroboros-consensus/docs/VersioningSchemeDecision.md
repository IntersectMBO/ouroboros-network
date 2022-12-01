# Choosing a Versioning Schemes

This document records our discussions around choosing how to version our packages.

In its current state, the document is primarily focused on one question: for each individual package, how should the version listed in the .cabal file on the `master` branch relate to the version in the .cabal file in the latest release?
In particular, I think this question is almost entirely independent of usings `git` branches versus `git` tags, SemVer versus PVP, etc.
This document should eventually specify that level of detail as well.

## Desiderata

- *Conventional Version Number*.
  We'd like our release versions to have the standard shape of `A.B.C`, where in particular increments of `C` principally represent bugfixes.

- *Simplicity and Familiarity*.
  We'd like the versioning scheme to be simple to explain, and ideally already well-established.

- *Ease of execution*.
  We'd like the process of cutting a release to merely involve following a very simple checklist.
  We'd like it to require as few inputs, discussions, decisions, etc.

- *Distinguished development versions*.
  We'd like to distinguish between the two possible semantics of a version number.

    - The version of a released thing identifies some immutable thing, which is always somehow more refined than any release that has a lesser version number.

    - A development version refers to some _mutable_ thing that is improving during the time between two releases, eg the version on the `master` branch.
      IE there will usually be multiple different commits that all have the same development version number.

## Proposal Simplest

To cut a new significant release, merely create branch `release/foo-A.B.x` pointing at the desired commit on the `master` branch and also immediately create a subsequent `master` commit that advances all the appropriate versions.

## Proposal Parity

Minor versions will be odd for packages on the `master` branch and even for releases (like the GHC Team's scheme).
To cut a new significant release, create branch `release/foo-A.B.x` pointing at the desired `master` commit and then add a commit to `release/foo-A.B.x` that bumps all versions to the next even number, and also immediately create a subsequent `master` commit that advances all the appropriate versions to the next odd number.

## Proposal NonZero

Master always has degenerate versions on it: everything is version `0`.
To cut a new significant release, create branch `release/foo-A.B.x` pointing at the desired `master` commit and then add a commit to `release/foo-A.B.x` that advances all appropriate versions COMPARED TO their value in the previous release branch.

## Proposal Dimension

FYI

```
Prelude Data.Version> makeVersion [1,2,0] `compare` makeVersion [1,2]
GT
```

Master versions only have two dimensions: `A.B`.
Release versions have at least three `A.B.C`, where `C` can be `0`.
To cut a new significant release, create branch `release/foo-A.B.x` pointing at the desired `master` commit and then add a `.0` `C` dimension to the existing `A.B` and immediately create a subsequent `master` commit that advances all appropriate versions to the next greater `A.B` pair.
