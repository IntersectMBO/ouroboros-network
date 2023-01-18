# How Consensus Makes Releases

This document explains how the Consensus team uses Pull Requests (PRs), version numbers, and branches to prepare releases of our packages.

Let us assume our repository contains two packages FOO and BAR.
Two is enough to explain the complexities.
The resulting rules can be generalized to any number of packages, as explained at the end of the document.

*Remark*.
If you disagree with the decisions in this document, please consider reviewing our brainstorming notes and discussions, available at [this document's PR](https://github.com/input-output-hk/ouroboros-network/pull/4207).
Even if you choose not to do so, please share your concern with us.

## Notation

In this document, we will assume each version number is a MAJOR.MINOR.PATCH triple.
This is easy to generalize to other conventional notations, such as [the PVP](https://pvp.haskell.org/)'s MAJOR.MAJOR.MINOR.PATCH quadruples.

We refer throughout this document to _the main branch_ (aka `master`, aka `main`).

If you search for "RULE:" in this document, you'll find the key statements.

## Rules for Branches

The Consensus team will sometimes be maintaining multiple MAJOR.MINOR versions of the same package simultaneously.
EG If we were currently maintaining a 2.3 version and a 2.4 version simultaneously, then we might release FOO-2.4.1 on Monday, release FOO-2.3.7 on Wednesday, and release FOO-2.4.2 on Friday, etc.

- Whenever we're maintaining only the one greatest-ever MAJOR.MINOR version of a package, then all work on that package will happen on the main branch.
- Whenever we're also maintaining some lesser MAJOR.MINOR versions of a package, then some work for that package will also be happening on the respective _release branches_.

A _release branch_ is a branch dedicated to the maintenance of some older release of a package.

RULE: If we have released a package FOO-X.Y.Z, then the release-FOO-X.Y.x branch MUST exist and its first-parent history MUST include the commit released as FOO-X.Y.Z.
For example, when releasing version 2.3.0 of a package named FOO, we must create the branch named release-FOO-2.3.x.
And when we later release 2.3.1 of FOO, then we'd need to advance the release-FOO-2.3.x branch to point at the commit being released as FOO-2.3.1.
See the "Release Branch Example" below for a more thorough example.

There are two kinds of work on release branches.
Most of the time, that work is immitating some work that was done on the main branch.
EG we fixed a bug on the main branch and we're backporting that fix onto a release branch (hopefully it's a trivial cherry-pick).
Rarely, though, there will be fresh work done on a release branch.
EG Consider the following possible timeline.

- We release FOO-2.4.1 on Monday, and FOO-2.4.1 completely reworked some functionality.
- We then realize on Tuesday that there was a severe bug in the old functionality.
- We fix the bug by merging a PR that targets the FOO-2.3.x release branch (since the bug no longer exists on the main branch!)
- We release FOO-2.3.7 on Wednesday.

*Remark*.
Not every first-parent commit in the history of the release branch was announced as a release of a package.
We will be merging multiple PRs into the release branch in order to prepare the next release from it, so some commits will just be the intermediates between two releases.

*Remark*.
The release branch for the greatest MAJOR.MINOR version of a package is somewhat degenerate.
It's either equal to or a prefix of the main branch.
The only time it MUST be updated to the tip of the main branch is when we cut a release of the package from the main branch, to satisfy the rule above about all commits that were released as some version X.Y.Z being on the release-X.Y.x branch.

## Rules for PRs

We classify each PR as either a _fresh_ PR or as a _backport_ PR.

- A fresh PR is doing work that has never been done before (new feature, new bugfix, etc).
  Except in the rare circumstances discussed in the previous section, every fresh PR will target the main branch.
- The primary objective of a backport PR is merely to immitate some fresh PR.

The rules are then as follows.

- RULE: A fresh PR MUST target the main branch, if possible.
- RULE: A PR MUST NOT be merged into a branch unless it should be included in the next release from that branch.
- We maintain a changelog for each package (one for FOO and one for BAR).
- RULE: A PR MUST add a pending changelog entry for each package that it alters.
- RULE: Each pending changelog entry MUST at the very least classify the alteration to that package as a _patch_ level change (eg a bugfix), a _minor_ change (eg a non-breaking interface change), or a _major_ change (eg a breaking interface change).

*Remark*.
Notice that we do not allow merging a PR that should not be included the subsequent release.
This means some PR may be "done" but still should not be merged.
When work has been done "prematurely" in this way, it risks requiring duplication of some future work (eg introduces merge conflicts with other simultaneously active PRs).
Our choice here doesn't create any more duplication, it merely confines that duplication to having to rebase that premature PR.
Our choice also preserves the intuitive monotonic relationship between releases and the main and releases branches.

We also maintain our changelog in a specific way.

- RULE: We maintain pending changelog entries in the same way that [`scriv`](https://github.com/nedbat/scriv) does, so that they do not generate conflicts and are not lost when rebasing/cherry-picking/etc PRs.
- RULE: The pending entries MUST NOT assume any specific version number of the previous release or the next release, because the entry may be backported etc.

*Remark*.
Backporting a PR wouldn't generate any conflicts in the changelog entries (by `scriv`'s design), so the author won't necessarily be prompted to update explicit version numbers.
Hence we forbid them, via the review process.
To explicitly clarify: it's fine to refer to historical explicit version numbers, but not in such a way that assumes anything about how many or which versions are between that explicit version and the changelog entry that mentions it.

## Rules for Releases

Infinitely often, the Consensus team will decide that FOO and/or BAR are ready for a next release (either from the main branch or from a release branch).
IE We will eventually decide that the code of that package on the tip commit of a branch should be the code of our next release of that package from that branch.
We prepare that release as follows.

- For each package we are including in the release, we review its pending changelog entries.
- RULE: We update the declared version (ie in its `.cabal` file) of each package we are including in the release based on the content of those pending changelog entries.
    - (Note that there's at least one package, since otherwise we wouldn't be mkaing a release.)
    - (Note that there must be some alterations to each package we are including in the release, since otherwise we wouldn't be including it in the release---but mind the _bundles_ mentioned below.)
    - Let X.Y.Z be the version of the package currently declared on this branch.
    - RULE: If any alteration was major-level, then we bump to (X+1).0.0.
    - RULE: Else if any alteration was minor-level, then we bump to X.(Y+1).0.
    - RULE: Otherwise all alterations were patch-level, so we bump to X.Y.(Z+1).
- RULE: We merge one final PR, which MUST do exactly the following and nothing more.
    - RULE: It updates the versions of the packages being released as described above.
    - RULE: It flushes the pending changelog entries of each package being released into the `CHANGELOG.md` file for that package.
- RULE: We tag that resulting merge commit as release-PKG-A.B.C; one tag per package PKG that is being released (ie FOO and/or BAR).
- RULE: If C is 0, then we also create the release branch release-PKG-A.B.x from this new commit.
    - For example, when releasing version 2.3.0 of a package named FOO, we'd create the branch named release-FOO-2.3.x.
      See the "Release Branch Example" below.
- RULE: Finally, we announce this commit hash as the new release of these packages.
  EG We insert these package's new versions into Hackage, [CHaP](https://github.com/input-output-hk/cardano-haskell-packages), etc.

*Remark*.
To explicitly clarify: after a release of a package, there will be zero pending changelog entries for that package.
When making the release, those entries were first incorporated into the `CHANGELOG.md` file and then the individual files containing those pending entires (see `scriv`'s mechanism) were removed (a la `git rm`).
But there may be pending changelog entries of other packages, those that were not included in this release.

*Remark*.
This scheme allows for multiple commits to declare their package has the same version number as some released version, even if those commits have made alterations to the package.
This means the mapping from commit hash <-> released version number is unfortunately not one-to-one.
There is obviously some possibility for confusion there.
However, we think the probability is low: if users only retrieve our code from the package repositories that we insert our release into, then the mapping will be one-to-one (as enforced by those repositories).
Despite it being relatively easy to preclude this risk (add an extra .0 dimension to the just-released versions immediately after announcing them, remove it when preparing the next release, but immediately add it back, etc -- thus between-release commits are always versions MAJOR.MINOR.PATCH.0), we're choosing not to.
The mitigation is quite simple, but that extra version dimension is unfamiliar to the broader community and so is likely to cause confusion of its own.
We are, though, ready to try this mitigation if the lack of one-to-one mapping does end up causing enough confusion.

## Generalizing to more packages

In actuality, the Consensus team maintains N-many packages instead of just FOO and BAR.
We could apply the above rules exactly as is: one changelog per package, independent version numbers per package, one release branch per MAJOR.MINOR.0 release of each package, etc.
We see some downsides to that.

- That's a lot of bookkeeping; many PRs would have to create multiple changelog entries.
- There's no easy way to recall which versions of our various packages are compatible with one another.
- There are multiple changelogs that it might not be easy for downstream users to navigate (ie to know which one to check for the information they're wondering about).

We therefore partition our N-many packages into B-many _bundles_ where B < N.

- RULE: The packages within a bundle always share a single changelog and always declare the exact same version.
- RULE: So we apply the rules from the above sections to each package bundle instead of to each package independently.

The only refinement to the wording in the above sections is that we update the `.cabal` file's of each package in a bundle being released based on the bundle's changelog entries, _even if that package had no alterations since the previous release of that bundle from this branch_.
This means some packages will occasionally get vacuous versions bumps.
That's obnoxious to downstream users and would be easy to interpret as evidence the Consensus team has made a mistake in during that release.
However, we either mitigate this by choosing our bundle partitioning to make it unlikely (put coupled packages into the same bundle) or else accept the oddity of the vacuous version bumps as an acceptable cost for the bundling's other advantages.

## Some Concrete Examples

Some of the rules above are much easier to internalize via concrete examples.

### Release Branch Example

Consider just one package, FOO.
Suppose we have released the following versions: 1.0.0 and 2.0.0.
The commit history might look like the following linear chain, with the tags and branch pointers annotated.

```
D - the release of FOO-2.0.0 (tag: release-FOO-2.0.0, branch: release-FOO-2.0.x, branch: main)
C - replace that confusing feature with much nicer one
B - the release of FOO-1.0.0 (tag: release-FOO-1.0.0, branch: release-FOO-1.0.x)
A - ...
```

If we subsequently merge a PR that fixes a minor bug present in 2.0.0, it'd then look like this.

```
E - fix that off-by-one error (branch: main)
D - the release of FOO-2.0.0 (tag: release-FOO-2.0.0, branch: release-FOO-2.0.x)
C - replace that confusing feature with much nicer one
B - the release of FOO-1.0.0 (tag: release-FOO-1.0.0, branch: release-FOO-1.0.x)
A - ...
```

We could have also advanced release-FOO-2.0.x, but the relevent RULES above do not _require_ doing so until we release version 2.0.1.
Once we do that, we'd have the following.

```
F - the release of FOO-2.0.1 (tag: release-FOO-2.0.1, branch: release-FOO-2.0.x, branch: main)
E - fix that off-by-one error
D - the release of FOO-2.0.0 (tag: release-FOO-2.0.0)
C - replace that confusing feature with much nicer one
B - the release of FOO-1.0.0 (tag: release-FOO-1.0.0, branch: release-FOO-1.0.x)
A - ...
```

We must advance release-FOO-2.0.x to the F commit, because we released F as a version 2.0.X and so the RULE above requires that the F commit is included in the first-parent history of release-FOO-2.0.x.

Suppose we then find a bad bug in feature that commit C had replace.
We might want to fix it because our users aren't yet ready to integrate the new feature from 2.0 version into their code.
Thus, we'd merge a bugfix PR directly into the release-1.0.x branch.
Note that we should merge bugfixes into main branch and then backport them onto a release branch when we can, but in this example the buggy feature no longer exists on the main branch.

```
G - fix confusion in the old logic (branch: release-FOO-1.0.x)
B - the release of FOO-1.0.0 (tag: release-FOO-1.0.0)
A - ...
```

Once that patch passes validation, we could then release 1.0.1.

```
H - the release of FOO-1.0.1 (tag: release-FOO-1.0.1, branch: release-FOO-1.0.x)
G - fix confusion in the old logic
B - the release of FOO-1.0.0 (tag: release-FOO-1.0.0)
A - ...
```

Now suppose we could have easily backported E onto 1.0.x as well.
If we're making another release of the 1.0 version, our users would likely appreciate us including as many bugfixes as we can.

```
I - cherry-pick of E (branch: release-FOO-1.0.x)
H - the release of FOO-1.0.1 (tag: release-FOO-1.0.1)
G - fix confusion in the old logic
B - the release of FOO-1.0.0 (tag: release-FOO-1.0.0)
A - ...
```

And shortly the following.

```
J - the release of FOO-1.0.2 (tag: release-FOO-1.0.2, branch: release-FOO-1.0.x)
I - cherry-pick of E
H - the release of FOO-1.0.1 (tag: release-FOO-1.0.1)
G - fix confusion in the old logic
B - the release of FOO-1.0.0 (tag: release-FOO-1.0.0)
A - ...
```
