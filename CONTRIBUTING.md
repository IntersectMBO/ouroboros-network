Thank you for taking the time to contribute 🙌

The following is a set of guidelines for contributing to the KES Agent library
and utilities.
If you have suggestions on how to improve this document, please feel
free to [propose changes](#contributing-to-the-code) to it in a pull request.
Bear in mind that the document should remain simple.
See [the quick reference section](#quick-reference) if you are in a hurry.

# Documentation

Documentation should be a first-class citizen of the KES Agent code base. Your
improvement proposals are welcome.

We have two types of documentation:

- Markdown files, which can be found in the [docs](docs) directory. They
  contain information that is not strictly related to the code itself, such as
  getting started guides, references, tutorials, etc.
  They can be viewed in rendered form on github, e.g.
  https://github.com/input-output-hk/kes-agent/blob/master/doc/guide.markdown

- [Haddock][haddock-site] comments. They contain more low level information
  about the code and should be used as a reference, although we aim to provide
  some context navigation (cross referencing modules) on the module
  descriptions.

When starting to work on KES Agent, we recommend to take a look at
[the guide](https://github.com/input-output-hk/kes-agent/blob/master/doc/guide.markdown).

When somebody asks a question about the code, we should try to refer people to
the documentation. If no relevant entry exists, we should create it and submit a
pull request.

# Setting up the build tools

## Using `cabal`

# Building the project

To build all the packages in this repository run:

```sh
cabal build all
```

Specific executables can be executed through cabal:

``` sh
cabal run db-analyser
```

# Testing

To run all the tests in the package, use this command:

```sh
cabal test all
```

You can filter the tests to run and change test parameters by changing Tasty
settings; the easiest way to do that is to set environment variables for the
test run, like this:

```sh
TASTY_PATTERN="end-to-end" cabal test all
```

For further information on the test parameters available, see [the Tasty
package on Hackage](https://hackage.haskell.org/package/tasty).

# Contributing to the code

The following sections contain some guidelines that should be followed when
contributing to the Consensus code base. Please take some time to go through
them. We do not expect newcomers to adhere to these guidelines perfectly, and we
will guide you through the process when reviewing your pull request.

## Quick reference

This section contain guidelines on what to check when making a pull request.

- When bumping version bounds on the dependencies *it is not necessary* to
  increase the package version number. See [this
  section](#updating-the-dependencies-bounds).
- When you want to create a changelog entry, follow [this and the following
  section](docs/website/contents/for-developers/ReleaseProcess.md#installing-scriv).

## Following our git process

Our [git process](docs/website/contents/for-developers/GitProcess.md) describes
the `git` practices we encourage when working with the code in this repository.

## Updating the documentation

When submitting a pull request, please look update the relevant parts of the
documentation (see [this section](#documentation)).

## Following the style guide

We have a [Haskell style
guide](docs/website/contents/for-developers/StyleGuide.md) that should be
followed when writing code in Consensus. Our style guide is not set in stone,
and improvements are always welcome.

## Formatting the code

We use `fourmolu` for ensuring a consistent formatting style.

You run the included script before pushing; this is the same script that the CI
itself will use to verify the formatting style:

```bash
./scripts/fourmolize.sh
```

## Making and reviewing changes

Before you get started, be aware that this is a security-critical component of
a Cardano setup, and there are many subtle gotchas. Make sure you understand
the purpose of KES cryptography, mlocking, and the role the KES agent plays in
all this.

If you are planning to make substantial changes, it is **important** that you
contact the core developers first to discuss design considerations (See section
[Contacting the developers](#contacting-the-developers)). This will help
detecting any potential problems with the change in the design phase,
preventing misunderstandings and frustrations later on in the process.

We do not currently maintain a changelog. The project is currently in an
experimental stage, and may change considerably on a short notice as we start
integrating it with the greater Cardano ecosystem and getting feedback from
users.

## Updating dependencies (`index-state`)

Our Haskell packages come from two package repositories:
- Hackage
- [CHaP][chap] (which is essentially another Hackage)

The `index-state` of each repository is pinned to a particular time in
`cabal.project`. This tells Cabal to treat the repository as if it was the
specified time, ensuring reproducibility. If you want to use a package version
from repository X which was added after the pinned index state time, you need to
bump the index state for X. This is not a big deal, since all it does is change
what packages `cabal` considers to be available when doing solving, but it will
change what package versions cabal picks for the plan, and so will likely result
in significant recompilation, and potentially some breakage. That typically just
means that we need to fix the breakage (increasing the lower-bound on the
problematic package if fix is not backward compatible), or delay that work and
instead decrease the upper-bound on the problematic package for now.

## Updating the dependencies bounds

Sometimes, when creating pull requests to [CHaP][chap], it is desirable to
loose/tighten certain dependencies bounds via a revision.

 - If your revision is about allowing a new version of a package, please update
   the version bound in a way that keeps the previously allowed versions, unless
   this is undesirable due to eg a bug in an earlier version.

   For example, if we have `kes-agent ^>= 1.1` and you want to also
   allow `1.2`, use `^>= 1.1 || ^>= 1.2`.

 - Keep in mind that `kes-agent` contains a library component that other
   Cardano packages depend on (particularly packages in `ouroboros-consensus`),
   and that there are shared dependencies between `kes-agent` and those other
   packages. This means that in order avoid tight coupling between the
   `kes-agent` library and those other packages, version bounds on those shared
   dependencies should be handled conservatively, since upgrading one of these
   dependencies across a breaking change boundary in `kes-agent` will require
   also upgrading it in the dependent package.

### Use of `source-repository-package`s

We *can* use Cabal's `source-repository-package` mechanism to pull in
un-released package versions. This can be useful when debugging/developing
across different repositories. However, we should not release our packages
to CHaP while we depend on a `source-repository-package` since downstream
consumers would not be able to build such package.

If we are stuck in a situation where we need a long-running fork of a
package, we should release it to CHaP instead (see the
[CHaP README](https://github.com/IntersectMBO/cardano-haskell-packages)
for more).

If you do add a temporary `source-repository-package` stanza, you need to
provide a `--sha256` comment in `cabal.project` so that Nix knows the hash
of the content. There are two relatively straightforward ways to do this:

1. The TOFU approach: put in the wrong hash and then Nix will tell you the
   correct one, which you can copy in.
2. Calculate the hash with `nix-shell -p nix-prefetch-git --run
   'nix-prefetch-git <URL> <COMMIT_HASH>'`

# Reporting a bug or requesting a feature

If you happen to encounter what you think is a bug or you wish there was some
feature that was added to KES Agent, please
[open](https://github.com/input-output-hk/kes-agent/issues/new/choose) an
issue in our [issue
tracker](https://github.com/input-output-hk/kes-agent/issues/).

# Submitting pull requests

We monitor the repository constantly so we should be aware if you open a
pull-request, however if some reasonable time passes and we miss your PR, feel
free to ping someone from the team.

# Contacting the developers

The core contributors to consensus codebase are:

-   [Tobias Dammers](https://github.com/tdammers)

# Code of conduct

See [Cardano engineering
handbook](https://github.com/input-output-hk/cardano-engineering-handbook/blob/main/CODE-OF-CONDUCT.md)'s
code of conduct.

[haddock-site]: https://haskell-haddock.readthedocs.io/latest/
[chap]: https://github.com/IntersectMBO/cardano-haskell-packages
