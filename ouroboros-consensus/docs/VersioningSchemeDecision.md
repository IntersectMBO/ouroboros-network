# Choosing a Versioning Schemes

This document records our discussions around choosing how to version our packages.

In its current state, the document is primarily focused on one question: for each individual package, how should the version listed in the .cabal file on the main integration branch relate to the version in the .cabal file in the latest release?
In particular, I think this question is almost entirely independent of usings `git` branches versus `git` tags for releases, SemVer versus PVP, etc.
This document should eventually specify that level of detail as well.

Given its current focus, Without loss of generality, suppose the repository contains only one package while considering the proposals below.

## Desiderata

- *Conventional Version Numbers*.
  We'd like our release versions to have the standard shape of `A.B.C`, where in particular increments of `C` principally represent bugfixes/documentation improvements/etc; we'll call that kind of change a _patch PR_.
  (Note, in the context of [the PVP](https://pvp.haskell.org/) eg, our `A` here would denote a pair.)

    - Our proposal here discuss when to increment `B`, but they never discuss incrementing `A`.
      You are of course free to decide any PR should advance to `(A+1).0...` or `A.(B+1)...` even if the proposed rules do not require it.

- *Simplicity and Familiarity*.
  We'd like the versioning scheme to be simple to explain and ideally already well-established.

- *Ease of Execution*.
  We'd like the process of cutting a release to merely involve following a very simple checklist.
  We'd like it to require as few inputs, discussions, decisions, etc as possible.

- *Distinguished Development Versions*.
  We'd like to distinguish between the two possible semantics of a version number.

    - The version of a released thing identifies some immutable thing, which is always somehow more refined than any thing---but especially another _released_ thing---that has a lesser version number.
      We denote this by "release version" below.

    - A development version refers to some mutable thing that is improving during the time between two releases, eg the version on the main integration branch.
      Note in particular that there will usually be multiple different commits that all have the same development version number.
      We denote this by "`main` version" below.

The main integration branch is typically named `main` in fresh GitHub repositories, so that's what we'll use in this document.

## Proposal AdvanceJustInTime

PRs do not alter the `main` version.

To cut a release from a commit COMMIT1 on `main` that declares version `A.B.C`, add to `main` a commit COMMIT2 that extends COMMIT1 merely to declare the version `A.(B+1).0` or `A.B.(C+1)` depending on what has changed on `main` since the previous release, and announce COMMIT2.

Cons:

- `main` versions and release versions are not distinguished.

## Proposal AdvanceImmediately

A patch PR doesn't alter the `main` version.
A more-than-patch PR must update the `main` version from `A.B.C` to `A.(B+1).0`.

To cut a release from a commit COMMIT on `main` that declares `A.B.C`, announce COMMIT.
Also immediately update the `main` version to `A.B.(C+1)`.

Cons:

- `main` versions and release versions are not distinguished.

- Some `main` commits will declare version `A.B.(C+1)`, even if that version is never officially released or is created only later by backporting _different_ patch commits to a previous release branch.

## Proposal Parity

Each `main` version `A.B.C` has an odd `B`.
Each release version `A.B.C` has an even `B`.
(This is similar to the GHC Team's scheme.)

PRs do not alter the `main` version.

To cut a release from a commit COMMIT on `main` that declares version `A.B` (where `B` is necessarily odd), announce a new non-`main` commit that extends COMMIT merely to declare version `A.(B+1)`.
Also immediately update the `main` version to `A.(B+2)`.

Cons:

- Commits on `main` that only include patches since the previous commit would still spuriously include the `A.(B+2)` increment.

## Proposal NonZero

Each `main` version is degenerate; eg it is always version `0`.
Each release version is the usual `A.B.C`.

PRs do not alter the `main` version.

To cut a release from a commit COMMIT on `main` (that necessarily declares version `0`), announce a new non-`main` commit that extends COMMIT merely to declare the version to be `A.(B+1).0`, or `A.B.(C+1)` depending on what has changed on `main` since the previous release.

Note that the degenerate versions could carry information.
EG they could be just a single number.
The only requirement is that they are inherently distinguished from release versions.

Cons:

- `main` versions would always be less than some older released version, which could cause confusion (among people and/or tools).

## Proposal Dimension

FYI

```
Prelude Data.Version> makeVersion [1,2] < makeVersion [1,2,0]
True
```

Each `main` version has only two dimensions: `A.B`.
Each release version has at least three `A.B.C`, where `C` can be `0`.

PRs do not alter the `main` version.

To cut a release from a commit COMMIT on `main` that declares version `A.B`, announce a new non-`main` commit that extends COMMIT merely to declare version `A.B.0`.
Also immediately update the `main` version to `A.(B+1)`.

## Proposal Dimension24

Proposal Dimension above has the downside that it incur spurious increments of `A.B` when the only differences between two releases were patch PRs.
The following enrichment adds the minimal amount of additional complexity to avoid that without losing any invariants.

Each release version is three-dimensional, `A.B.C`.
Each `main` version is either two-dimensional `A.B` or four-dimensional `A.B.C.9001`;
it's four-dimensional when the next release is just a patch, and it's two-dimensional when the next release is more than a patch.

A patch PR doesn't alter the `main` version.
A more-than-patch PR must update the `main` version as follows.

```
  A.B.C.9001 -> A.(B+1)
  A.B        -> A.B
```

To cut a release from a commit COMMIT on `main` that declares version `A.B`, announce a new non-`main` commit that extends COMMIT merely to declare version `A.B.0`.
Also immediately update the `main` version to `A.B.0.9001`.

To cut a release from a commit COMMIT on `main` that declares version `A.B.C.9001`, announce a new non-`main` commit that extends COMMIT merely to declare version `A.B.(C+1)`.
Also immediately update the `main` version to `A.B.(C+1).9001`.

The following labelled transition system rules captures how the `main` version evolves.

```
  st          --------[patch PR]-------> st

  A.B.C.9001  ---[more-than-patch PR]--> A.(B+1)
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

This only thing this scheme does not track is changes between `main` versions beyond the escalation from patch-level changes to more-than-patch level.
