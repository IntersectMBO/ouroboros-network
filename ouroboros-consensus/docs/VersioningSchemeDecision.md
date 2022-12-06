# Choosing a Versioning Schemes

This document records our discussions around choosing how to version our packages.

In its current state, the document is primarily focused on one question: for each individual package, how should the version listed in the `.cabal` file on the main integration branch relate to the version in the `.cabal` file in the latest release?
In particular, I think this question is almost entirely independent of usings `git` branches versus `git` tags for releases, SemVer versus PVP, etc.
This document should eventually specify that level of detail as well.

Given its current focus, without loss of generality, suppose the repository contains only one package while considering the proposals below.

## Desiderata

- *Conventional Version Numbers*.
  We'd like our release versions to have the standard shape of `A.B.C`, and the standard rules that `A` must increase for breaking changes, `B` must increase for backwards-compatible changes, and `C` must increase for anything else (eg bugfixes/documentation/etc aka "patches").

    - Some of our proposals below don't care about the distinction between `A` and `B`.
      Instead of repeating the semantics of `A` and `B` in each proposal, we refer to `next(A.B)` with the intention that the developer's determine whether to increment `A` or `B`.

    - You are of course free to decide any PR should increment `A` or `B` even if the proposed rules do not require it.
      Ultimately, the only risk of such spurious increases is confusing/inconveniencing downstream users.

    - Note, in the context of [the PVP](https://pvp.haskell.org/) eg, our `A` dimension here would itself denote a pair.

- *Simplicity and Familiarity*.
  We'd like the versioning scheme to be simple to explain and ideally already well-established.

- *Ease of Execution*.
  We'd like the process of cutting a release to merely involve following a very simple checklist.
  We'd like it to require as few inputs, discussions, decisions, etc as possible.

- *Distinguished Development Versions*.
  We'd like to distinguish between the two possible semantics of a version number.

    - The version of a released thing identifies some immutable thing, which is always somehow more refined than any thing---but especially another _released_ thing---that has a lesser version number.
      We denote this kind of version by "release version" below.

    - A development version refers to some mutable thing that is improving during the time between two releases, eg the version on the main integration branch.
      Note in particular that there will usually be multiple different commits that all have the same development version number.
      We denote this kind of version by "`main` version" below.

The main integration branch is typically named `main` in fresh GitHub repositories, so that's what we'll use in this document.

## Proposal RisingEdge

PRs do not alter the `main` version.

To cut a release from a commit COMMIT1 on `main` that declares version `A.B.C`, add to `main` a commit COMMIT2 that extends COMMIT1 merely to declare the version `next(A.B).0` or `A.B.(C+1)` depending on what has changed on `main` since the previous release, and announce COMMIT2.

Cons:

- `main` versions and release versions are not distinguished.

- Cutting a release requires assessing all the changes on `main` since the last release.

## Proposal FallingEdgePatch

A `C`-level PR doesn't alter the `main` version.
Each `A`-level PR must update the `main` version from `A.B.C` to `(A+1).B.0` unless `A.0.0` is already greater than the previous release.
Each `B`-level PR must update the `main` version from `A.B.C` to `A.(B+1).0` unless `A.B.0` is already greater than the previous release.

To cut a release from a commit COMMIT on `main` that declares `A.B.C`, announce COMMIT.
Also immediately update the `main` version to `A.B.(C+1)`.

Cons:

- `main` versions and release versions are not distinguished.

- Some `main` commits will declare version `A.B.(C+1)` even if that version is never officially released or is created only later by backporting _different_ (patch) commits to a previous release branch.

## Proposal Parity

Each `main` version `A.B.C` has an odd `B`.
Each release version `A.B.C` has an even `B`.
This is similar to the GHC Team's scheme.

PRs do not alter the `main` version.

To cut a release from a commit COMMIT on `main` that declares version `A.B` (where `B` is necessarily odd), announce a new non-`main` commit that extends COMMIT merely to declare version `X.Y = next(A.B)` (where `Y` is necessarily even).
Also immediately update the `main` version to `X.(Y+1)`.

Cons:

- Commits on `main` that only include patches since the previous release would still spuriously include the `Y+1` increment in the `B` dimension.

## Proposal NonZero

Each `main` version is degenerate; eg it is always version `0`.
Each release version is the usual `A.B.C`.

PRs do not alter the `main` version.

To cut a release from a commit COMMIT on `main` (that necessarily declares version `0`), announce a new non-`main` commit that extends COMMIT merely to declare the version to be `next(A.B).0`, or `A.B.(C+1)` depending on what has changed on `main` since the previous release.

Note that the degenerate versions could carry information.
EG they could be just a single number.
The only requirement is that they are inherently distinguished from release versions.

Cons:

- `main` versions would always be less than some older released version, which could cause confusion (among people and/or tools).

- Cutting a release requires assessing all the changes on `main` since the last release.

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
Also immediately update the `main` version to `next(A.B)`.

Pros:

- It enforces all desired invariants.

- It's a very mechanical state machine, easy to execute and also easy to immediately recognize which state it's in.

Cons:

- The multi-sorted state transition system prevents any explanation from being comparatively small.

- This scheme is certainly not already well-established!

- It incurs spurious increments of `A.B` when the only differences between two releases were patch PRs.

## Proposal Dimension124

Proposal Dimension above has the downside that it incurs spurious increments of `A.B` when the only differences between two releases were patch PRs.
The following enrichment adds the minimal amount of additional complexity to avoid that without losing any invariants.

Each `main` version is `A`, `A.B` where `B>0`, or `A.B.C.2718`; it is never three-dimensional.
Each release version is the usual `A.B.C`.
`2718` is just a recognizable magic number; it's so large it's unlikely to come up in actual versioning and it's the first digits of _e_, which is related to _growth_, which is what the `main` branch is for.

PRs alter the `main` version as indicated in the following diagram.

```
  A          --------[A-level PR]---------> A
  A          --------[B-level PR]---------> A
  A          --------[C-level PR]---------> A

  A.B        --------[A-level PR]---------> A+1
  A.B        --------[B-level PR]---------> A.B
  A.B        --------[C-level PR]---------> A.B

  A.B.C.2718 --------[A-level PR]---------> A+1
  A.B.C.2718 --------[B-level PR]---------> A.(B+1)
  A.B.C.2718 --------[C-level PR]---------> A.B.C.2718
```

To cut a release from a commit COMMIT on `main` that declares version `A`, announce a new non-`main` commit that extends COMMIT merely to declare version `A.0.0`.
Also immediately update the `main` version to `A.0.0.2718`.

To cut a release from a commit COMMIT on `main` that declares version `A.B`, announce a new non-`main` commit that extends COMMIT merely to declare version `A.B.0`.
Also immediately update the `main` version to `A.B.0.2718`.

To cut a release from a commit COMMIT on `main` that declares version `A.B.C.2718`, announce a new non-`main` commit that extends COMMIT merely to declare version `A.B.(C+1)`.
Also immediately update the `main` version to `A.B.(C+1).2718`.

The above is summarized in the following diagram, which relies on this legend depicting the semantics of the one kind of node (state) and the semantics of the two kinds of edges (transitions).

```mermaid
%%{init: {'themeVariables': { 'edgeLabelBackground': 'black', 'clusterBkg': 'transparent', 'clusterBorder': 'transparent' }}}%%
graph LR
    %% colors from https://davidmathlogic.com/colorblind/
    %% theming syntax from https://mermaid-js.github.io/mermaid/#/theming?id=customizing-themes-with-themevariables

    %% invisible nodes
    preLEGEND[ ]; style preLEGEND height:0px
    preLEGEND1[ ]; style preLEGEND1 height:0px
    preLEGEND2[ ]; style preLEGEND2 height:0px

    LEGEND["what the subsequent version on main branch must immediately be"]

    preLEGEND --- preLEGEND1 --> |cut the release X.Y.Z| LEGEND
    linkStyle 0 stroke:transparent
    linkStyle 1 stroke:#E1BE6A

    preLEGEND --- preLEGEND2 --> |merge a PR| LEGEND
    linkStyle 2 stroke:transparent
    linkStyle 3 stroke:#40B0A6
```

This diagram is a schema: one instance exists for every concrete value of the release version `A.B.C`.
Crucially: the result state of each `release X.Y.Z` transition also exists in its own instance of the schema.
Thus, this schema inductively defines an exhaustive (infinite) state machine.

```mermaid
%%{init: {'themeVariables': { 'edgeLabelBackground': 'black', 'clusterBkg': 'transparent', 'clusterBorder': 'transparent' }}}%%
graph LR
    %% colors from https://davidmathlogic.com/colorblind/
    %% theming syntax from https://mermaid-js.github.io/mermaid/#/theming?id=customizing-themes-with-themevariables

    START[ ]; style START height:0px

    %% cause the layout algorithm to group up the principal nodes, those states between release A.B.C and whatever the next release is
    subgraph " "
        cright["A.B.C.2718"]
        bplusleft["A.(B+1)"]
        aplusleft["A+1"]
    end

    %% invisible intermediate nodes that force each release transition to reach outside of the above subgraph
    RELEASEA[ ]; style RELEASEA height:0px
    RELEASEB[ ]; style RELEASEB height:0px
    RELEASEC[ ]; style RELEASEC height:0px

    %% release transitions
    START     --> |release A.B.C| cright
    cright    --- RELEASEC --> |"release A.B.(C+1)"| cplus["A.B.(C+1).2718"]
    bplusleft --- RELEASEB --> |"release A.(B+1).0"| bplus["A.(B+1).0.2718"]
    aplusleft --- RELEASEA --> |"release (A+1).0.0"| aplus["(A+1).0.0.2718"]
    linkStyle 0 stroke:#E1BE6A
    linkStyle 1 stroke:#E1BE6A
    linkStyle 2 stroke:#E1BE6A
    linkStyle 3 stroke:#E1BE6A
    linkStyle 4 stroke:#E1BE6A
    linkStyle 5 stroke:#E1BE6A
    linkStyle 6 stroke:#E1BE6A

    %% merge transitions
    cright    --> |bugfix PR| cright
    cright    --> |backwards-compat PR| bplusleft
    cright    --> |backwards-incompat PR| aplusleft
    bplusleft --> |bugfix or backwards-compat PR| bplusleft
    bplusleft --> |backwards-incompat PR| aplusleft
    aplusleft --> |any PR| aplusleft
    linkStyle 7 stroke:#40B0A6
    linkStyle 8 stroke:#40B0A6
    linkStyle 9 stroke:#40B0A6
    linkStyle 10 stroke:#40B0A6
    linkStyle 11 stroke:#40B0A6
    linkStyle 12 stroke:#40B0A6
```

The induced state machine ensures that each version is as meaningful as we'd like because `P <= Q` on every PR merge and `R < X.Y.Z < S` on every release.

```mermaid
%%{init: {'themeVariables': { 'edgeLabelBackground': 'black', 'clusterBkg': 'transparent', 'clusterBorder': 'transparent' }}}%%
graph LR
    %% colors from https://davidmathlogic.com/colorblind/
    %% theming syntax from https://mermaid-js.github.io/mermaid/#/theming?id=customizing-themes-with-themevariables

    L1[P] --> |merge a PR| R1[Q]
    linkStyle 0 stroke:#40B0A6

    L2[R] --> |cut the release X.Y.Z| R2[S]
    linkStyle 1 stroke:#E1BE6A
```

Pros:

- It enforces all desired invariants.

- It's a very mechanical state machine, easy to execute and also easy to immediately recognize which state it's in.

- It allows for the natural minimal progression of release versions.

Cons:

- The multi-sorted state transition system prevents any explanation from being comparatively small.

- This scheme is certainly not already well-established!

Notes:

- The above state machine is natural for a monorepo, since you wouldn't make a release if nothing had changed.
  However, in a polyrepo, you might regret updating all the revisions to `A.B.C.2718` if you want to do your next release when some of the packages haven't changed.
  You can slightly complicate the state machine to avoid that happening.

    - Split the `A.B.C.2718` node into two: `A.B.C.2718` and just `A.B.C`.

    - Transition from `A.B.C` to `A.B.C.2718` only when merging a bugfix/etc PR.
      Otherwise `A.B.C` has the some outgoing transitions as `A.B.C.2718`.

    - Change all of the `release X.Y.Z` transitions to target `X.Y.Z`.
      Thus, you set the `main` version to the release version immediately after cutting a release, but _any_ PR affecting that package should increase its `main` version to at least `X.Y.Z.2718`.

## Proposal Redimensional

What should the version on your main branch as of merging a PR that changes your package?
The code is now more advanced than the previous release---call that `A.B.C`---so it should have a greater version than `A.B.C`.
That's either `A.B.(C+1)` for a bugfix, `A.(B+1).0` for new functionality, or `(A+1).0.0` for a breaking change.
Let's suppose the PR adds a feature and so sets the version to `A.(B+1).0`.
As a result, the resulting merge commit that declares that it defines version `A.(B+1).0` of the package.

If you were to now announce that commit as version `A.(B+1).0`, then all will be intuitive and risk no surprises.
However, suppose you never announce that commit; perhaps it's too incremental to trouble your users with a dedicated release.
Instead, the next thing you do is merge another feature PR.
There are three options for how that second feature PR should affect the version.
- The second PR bumps the version to `A.(B+2).0`.
  This would be unintuitive/surprising because now the timeline of releases will skip version `A.(B+1).0` of our package.
- The second PR bumps the version to `A.(B+1).1`.
  This would be very unintuitive/suprising for the same reason as `A.(B+2).0` and also because it violates the usual semantics of version components.
- The second PR leaves the version at `A.(B+1).0`.
  This would be unintuitive/surprising because there are now two commits (forever accessible via the main branch) that both claim to define version `A.(B+1).0` of the package.
  (Many readers might be unalarmed about that, but I suspect acceptance/the status quo is partly why.)

The above thought experiment generalizes to the following claim.

> Without a distinction between development versions and release versions, the maintainer must continually choose from the following unappealing options.
>
> - Have multiple commits (forever accessible via the main branch) that all claim to define the same version of the package.
> - Skip over some version numbers in the release timeline.
> - Release after every PR that changes the package.

How, then, to distinguish between dev versions and release versions?
The core requirements are as follows.
- Every release version should be familiar, eg the standard `A.B.C`.
  (In the context of [the PVP](https://pvp.haskell.org/) eg, this `A` component would itself contain two inner components.)
- Both must be compatible with standard tools.
  For example, standard tools must be able to parse both dev versions and release versions.
- The repository as of the merge commit of a PR must not declare release versions for packages that the PR changed.
- A package must have a release version as of a commit announced as a release of that package.
Thus the first PR that alters a package after the previous release must change that package's declared version from the release version to a dev version.

The remaining question is what should that dev version be for a specific PR?
Suppose the previous release was `2.2.2`.
Also suppose this PR is just a bugfix.
A promising idea is to use `2.2.2.0` as the new dev version.
- It has one more component than the release version, so it's distinct.
- Standard tools typically support a variable number of components.
- Moreover, `2.2.2.0` is greater than `2.2.2`, which seems intuitive: it's more correct than `2.2.2`.
- Similarily, it's less than all the possible next releases: `2.2.3`, `2.3.0`, and `3.0.0`.

Suppose instead the PR added a feature instead of fixing a bug.
Is it still the case that new version should be `2.2.2.0`?
It does seem surprising that a version `2.2.2.0` would have more _features_ than version `2.2.2`, since they both start with `2.2`.
One could dismiss this, arguing that `2.2.2.0` is obviously a dev version (it has the extra component), and so people who understand the distinction between dev versions and release versions would know not to infer anything more than "2.2.2.0 is at least one PR ahead of version 2.2.2".

However, standard tools do not understand the distinction!
In particular, `Cabal` will happily pick `2.2.2.0` when the downstream user has written the constraint as `>= 2.2.2 && < 2.3.0`.
In this case, because the commit that defines `2.2.2.0` adds a feature, `Cabal`'s naive inference based on `2.2.2.0 < 2.3.0` is unsound.
One could also dismiss this concern about tooling, though, because only devs (here or downstream) should ever be building dev versions, and they should only do so explicitly (eg building from source by building a git worktree or by listing a specific git commit as the dependency).
As long as no one installs a dev version package into a repository, there's no risk of a tool that picks amongst versions from some repository even considering a dev version.
Dev versions are never _released_, and so should never be in any public package repository such as Hackage or [CHaP](https://github.com/input-output-hk/cardano-haskell-packages).
The most likely way we can imagine a dev version might end up in a repository would be if a developer/user who doesn't know any better builds from source and installs a dev version into their machine's local package database.
That single risk seems manageable.

This suggests the following proposal.
- Each release version is `A.B.C`.
- Each dev version is `A.B.C.D`.
- Any PR that alters a package in a way that should affect the next release version should update its version as follows.
    - If the version was `A.B.C` before the PR, bump it to `A.B.C.0`.
    - If the version was `A.B.C.D` before the PR, bump it to `A.B.C.(D+1)`.
        - This rule isn't fundamental, but it ensures that every PR touches the version, which in turn ensures `git` will raise merge conflicts for backports/rebases/cherry-picking/etc.
          Those conflicts force the developer to make adjustments to versions.
          Those conflicts are necessary and also trivial to resolve but would be easy to overlook without this rule leveraging `git` to remind us.
          Additionally, this means that no two code-changing commits in the subset of a single branch will declare the same version---or set of versions if the repo has multiple packages---which seems intuitive.
        - Similar conflicts will also require updating open version-altering PRs after each version-altering PR is merged.
          Con: that will likely be tedious.
          Pro: it will likely lead to more linear integration branch history: you might was well rebase while you're updating the version bumps.
        - If you want `D` to exactly count the number of relevant PRs since the previous release, then you'll need to require every merge is a [fast-forward merge](https://git-scm.com/docs/git-merge#_fast_forward_merge); see [`git merge --ff-only`](https://git-scm.com/docs/git-merge#Documentation/git-merge.txt---ff-only).
- Cut a new release as follows.
    - Merge a fresh PR that doesn't alter the package but does bump dev version `A.B.C.D` to the next release version, depending on the contents of the relevant PRs since the previous release:
      `(A+1).0.0` if there were breaking changes, else `A.(B+1).0` if there were some new features, else `A.B.(C+1)`.
      (Note that `.D` is present, so at least one version-influencing change was made.
      If the version in the commit was still `A.B.C`, why are you announcing a new release?)
    - Announce that commit as a release of the packages that have changed since the previous release.
      (In a monorepo, there's just the one.
      But in a polyrepo, there may be packages that haven't changed since the previous release.)

## Proposal Redimensional124

Recall that the motivation for Proposal Redimensional dismisses the concern that people and/or tools will be confused by `A.B.C.D` possible having more features or breaking changes compared to `A.B.C`.
Those not ready to dismiss that concern can consider the following proposal, which considers enough additional shapes of dev version to eliminate that risk.
- Each release version is `A.B.C`.
- Each dev version is either `A`, `A.B`, or `A.B.C.0`.
    - (We have fixed `.D` at `.0`, for consistency with the other two dev version shapes, which don't have the degree of freedom.
      This unfortunately does increase the risks of backports/rebases/cherry-picking/etc failing to bump the version.)
    - (It may be helpful to recognize `A.B.C.0` as `A.B.C + ϵ = A.B.(C+1) - ϵ`, `A.B` as `A.B.0 - ϵ`, and `A` as `A.0.0 - ϵ`, where ϵ stands for a 3-vector ["bigger than [`0.0.0`], but smaller than all the [others]"](https://en.wikipedia.org/wiki/Greek_letters_used_in_mathematics,_science,_and_engineering).)
- Any PR that alters a package in a way that should affect the next release version should update its version as follows.
    - If the version was `A` before the PR, leave it.
    - Else if the version was `A.B` before the PR:
        - If the PR makes a breaking change, bump it to `(A+1)`.
        - Else leave it.
    - Else if the version was `A.B.C.0` before the PR:
        - If the PR makes a breaking change, bump it to `(A+1)`.
        - Else if the PR adds new functionality, bump it to `A.(B+1)`.
        - Else leave it.
    - Else the version must have been `A.B.C` before the PR.
        - If the PR makes a breaking change, bump it to `(A+1)`.
        - Else if the PR adds new functionality, bump it to `A.(B+1)`.
        - Else bump it to `A.B.C.0`.
- Cut a new release as follows.
    - Merge a fresh PR that doesn't alter the package but does bump dev version to the next release version, as follows.
        - Bump `A` to `A.0.0`.
        - Bump `A.B` to `A.B.0`.
        - Bump `A.B.C.0` to `A.B.(C+1)`.
    - Announce that commit as a release of the packages that have changed since the previous release.
      (In a monorepo, there's just the one.
      But in a polyrepo, there may be packages that haven't changed since the previous release.)
    - Announce that commit.
