TODO "WLOG, suppose the repository contains only one package"

TODO simpler formula for "release a non-`master` commit that declares version ..."

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

- *Ease of Execution*.
  We'd like the process of cutting a release to merely involve following a very simple checklist.
  We'd like it to require as few inputs, discussions, decisions, etc.

- *Distinguished Development Versions*.
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

To cut a new significant release, tag a commit _not on `master`_ that advances all appropriate versions COMPARED TO their value in the previous release branch.

Note that the degenerate versions could carry information.
EG they could be just a single number.
The only requirement is that they are inherently distinguished from release versions.

## Proposal Dimension

FYI

```
Prelude Data.Version> makeVersion [1,2,0] `compare` makeVersion [1,2]
GT
```

Master versions only have two dimensions: `A.B`.
Release versions have at least three `A.B.C`, where `C` can be `0`.
To cut a new significant release, create branch `release/foo-A.B.x` pointing at the desired `master` commit and then add a `.0` `C` dimension to the existing `A.B` and immediately create a subsequent `master` commit that advances all appropriate versions to the next greater `A.B` pair.

## Proposal Dimension24

Proposal Dimension above has the downside of spurious increments of Major.Minor.
The following enrichment adds the minimal amount of additional complexity to avoid that without losing any invariants.

Release versions are A.B.C = Major.Minor.Patch (or PVP's Major.Major.Minor.Patch)

Master versions are either A.B = Major.Minor OR A.B.C.Z = Major.Minor.Patch.9001;
it's four dimensional when the next release is just a patch, and it's two dimensional when the next release is more than a patch.

Any PR that just fixes bug doesn't alter the master versions.
And any PR that does more than just fix a bug must also update the master versions as follows.

  A.B.C.9001 -> next(A.B)
  A.B        -> A.B

Of course, you're also free to choose that any PR sets the version to (A+1+n).(B+1+n) even if the above rules do not require it.

To cut a significant release from master version A.B, release a non-`master` commit that declares version A.B.0.
Also immediately update the master version to A.B.0.9001.

To cut a significant release from master version Right A.B.C.9001, release a non-`master` commit that declares version A.B.(C+1).
Also immediately update the master version to Right A.B.(C+1).9001.

The following labelled transition system rules captures how the master version evolves.

```
  st          --------[patch PR]-------> st

  A.B.C.9001  ---[more-than-patch PR]--> next(A.B)
  A.B         ---[more-than-patch PR]--> A.B

  A.B.C.9001  ---[release A.B.(C+1)]---> A.B.(C+1).9001
  A.B         -----[release A.B.0]-----> A.B.0.9001
```

Pro:

- It enforces all desired invariants.

- It's a very mechanical state machine, easy to execute and also easy to immediately recognize which state it's in.

Cons:

- The state machine prevents any explanation from being comparatively small.

- This scheme is certainly not already well-established!

This only thing this scheme does not track bugfix changes within master versions;
but that isn't even on of our desiderata.
