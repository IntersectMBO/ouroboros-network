# Contributing to Ouroboros-Network

## Setting up and working with development tools

### Building with Cabal

To setup development tools (`ghc`, `cabal` & `hls` - Haskell Language Server)
we advise you to use [`ghcup`].

The project can be built with a recent enough version of `cabal` and `ghc` (see
the GitHub Action [workflow](.github/workflows/build.yml) file which versions
we currently support.

If you use a different compiler by default, you can pin the compiler version in
`cabal.project.local` file, see [here][cabal-with-compiler].  You might want to
use a few other options which make the compilation faster if you want to
develop & contribute to this repository, e.g.

```cabal.project
jobs:          2
-- You might want to adjust if you have more processors, note that there are
-- two levels of concurrency when building with `cabal`: this option decides how
-- many packages can be built at the same time, while `GHC`'s option `-j`
-- guards how many modules in a single package can be built concurrently (see
-- below how it can be set).
-- NOTE: for the settings in this file to be effective you need to have
-- 4 real cores.
optimization:  0
-- Building non-optimised code helps to build quicker but some of the tests
-- will take much longer to run.
documentation: False
-- Documentation is built by the CI, you will get feedback if your haddocks are
-- unparsable.
tests:         True
benchmarks:    True

package ouroboros-network-framework
    ghc-options: -j2
package ouroboros-network
    ghc-options: -j2
package ouroboros-consensus
    ghc-options: -j2
```

### Building with Nix

You can also build all the packages with `nix`.  To install `nix` on your
system please follow [this guide][nix-setup] which contains some `IOG` specific
`nix` configuration options (e.g. our `nix` cache, which considerably speeds up
bootstrapping the project); the official
[guide](https://nixos.org/download.html) might be helpful too.

To build all the required jobs (which are necessary to pass through CI), you can run:

```sh
nix build -j auto .\#hydraJobs.required
```

To inspect what can be build use `nix repl` , for example:
```
nix-repl> :lf .
nix-repl> hydraJobs.<TAB>
nix-repl> hydraJobs.
hydraJobs.aarch64-darwin  hydraJobs.x86_64-darwin   hydraJobs.x86_64-linux
```

In various packages, we use `CPP` pragmas to compile different code depending
on the target architecture.  Using `haskell.nix` cross-compilation pipeline
is very helpful to diagnose build time compiler errors.

#### Cross Compilation

Nix allows to build Windows native executables on Linux, e.g. to build
`network-mux:lib:network-mux` component one can run this command:

```bash
nix build .\#hydraJobs.x86_64-linux.ghc810-x86_64-w64-mingw32.packages.network-mux:lib:network-mux
```

Not all components are available in cross-compilation right now.  All the
test components are disabled in `./scripts/ci/cabal.project.local.Windows.CrossCompile`.

### Running tests with cabal

To run all tests from a particular component use the following command, in this
example `ouroboros-network`:

```sh
cabal run ouroboros-network:test
```

We use [`tasty`] library which exposes an interface to list & isolate
a test or group of tests to run.   The following command will list all the
tests of the `ouroboros-network:test` component:

```
$ cabal run ouroboros-network:test -- -l
ouroboros-network.ChainProducerState.Test Arbitrary instances.ChainProducerStateForkTest's generator
ouroboros-network.ChainProducerState.Test Arbitrary instances.ChainProducerStateForkTest's shrinker
ouroboros-network.ChainProducerState.check initial follower state
...
```

To match only tests which contain particular string:
```
$ cabal run ouroboros-network:test -- -p '/governor no livelock/' -l
ouroboros-network.Ouroboros.Network.PeerSelection.governor no livelock
ouroboros-network.Ouroboros.Network.PeerSelection.races.governor no livelock

$ cabal run ouroboros-network:test -- -p '/PeerSelection.governor no livelock/' -l
ouroboros-network.Ouroboros.Network.PeerSelection.governor no livelock
```

You can also use the test hierarchy, e.g. the last test can also be selected with:
```
$ cabal run ouroboros-network:test -- -p '$2 == "Ouroboros.Network.PeerSelection" && $3 == "governor no livelock"' -l
ouroboros-network.Ouroboros.Network.PeerSelection.governor no livelock
```

If you want to run selected tests just avoid the `-l` (`--list-tests`) switch.

If in doubt you can use `-h` or visit [tasty documentation][tasty-options].

### Building & running tests with nix

`nix build -j auto .\#hydraJobs.required` will build and run all required
checks.  If you want to build only tests for a particular package, e.g.
`network-mux` package (on `linux`) use:

```sh
nix build -j auto .\#hydraJobs.x86_64-linux.packages.network-mux:test:test
```

The executable will be available at `./results/bin/` directory.  You can pass
to it the same options as in the previous section (the options after `--`).

## Documentation

Any contributions should be well documented.  APIs should have well-written
`haddocks`.  If a particular function expects a precondition to be satisfied it
should be explicitly mentioned.  The inline documentation is published at
<https://input-output-hk.github.io/ouroboros-network>.  When writing haddocks
it's always good to put oneself in the position of somebody who hasn't yet
interacted with your code changes.  It's good to explain the key design choices
as well as implementation-level comments.

If changes would modify any existing design the contributor might be expected
to be asked to also update the standalone documentation (written in `tex`).

### Building technical documentation

There are two documents in two directories:

* `./doc/network-design`
* `./doc/network-spec`

Either go to one of the directories and run `pdflatex` & `bibtex` (there's a
`Makefile` or a script to do that).  Note that in you need to install a `tex`
distribution on your system with all the necessary packages; or build it with
`nix` (which will take care about all the dependencies one needs):

```sh
# build network-design & network-spec
nix build -f default.nix network-docs
```

## Coding Standards

All contributed code should be well-tested.  Low-level networking code should
be tested both in simulation ([`io-sim`][io-sim-lib]) and in `IO` (which we run
on different architectures), while top-level code (e.g. the diffusion layer) is
only tested in simulation).  We use [`QuickCheck`], if you are new to property
based testing please check out one of the original John Hughes tutorials, e.g. [How
to Specify it!](https://www.youtube.com/watch?v=G0NUOst-53U).  We combine
`QuickCheck` with an in-house built [`io-sim`] library.  As a consequence,
almost all of our code is written in a polymorphic way using `io-classes` which
comes with [`io-sim`]: `io-classes` expose a very similar API that the `base`,
`async`, `stm` and `time` packages provides.

### Important Coding Remarks

* Please note that across this code base, we are using custom time functions
  which are measured in seconds not nano or microseconds as similarly named
  functions in `base` do.  This applies to:
  - `timeout`
  - `threadDelay`
  - `registerDelay`
  - `registerDelayCancellable` (which does not exist in `base`)
  They all use `DiffTime` rather than `Int`, so you can use fractional values
  if needed.

  Failing to notice this, might lead to bugs where delays which supposed to
  be in the order of seconds will be measured in months (`3*10^6` seconds ~ one
  month)!

### Style Guides

The network & consensus have slightly different style guides, see

* [network style guide](./docs/StyleGuide.md)
* [consensus style guide](https://github.com/input-output-hk/ouroboros-consensus/blob/main/docs/StyleGuide.md)

### Git History

Please take your time to clean and format your PR's git history with the
reviewer in mind.  Very often complex changes can be split into small
refactoring steps followed by new additions which are using 
refactorisation.  Please avoid including random changes in large commits
which make it difficult to review.  We recognise that formatting git history
takes time and effort, but we find it much easier to discuss & review the
changes, as well as rebase if there are any other complex changes merged in (or it
will make it easier for others to rebase on top of your committed changes).  If
you need to rebase your branch we prefer to rebase over merge (since then the
actually merged changes are more explicit).

Since the code base of `ouroboros-network` is quite large, we don't require
that every commit is buildable across all included packages.  You can update
upstream dependencies later in the commit history; although note that if you do
that you are limiting the way tools like `git-bisect` can be used, so please be
aware of that.  If you decide to do that, please indicate that in the commit
message.

### Commits

We find it quite helpful if each commit title starts with the component it
modifies.  Sometimes referring to package name is good enough, sometimes
something more specific like `outbound-governor` is more appropriate.  At this
stage, this is only advisory.  Please check <https://commit.style>.  Here's an
example from our own [git
history](https://github.com/input-output-hk/ouroboros-network/commit/96584fc66a8ea5c0625b2ad5c91959a3c909568c).

We require all commits to be signed, see [this guide][gh-signing-commits].

### Large Changes

If you are thinking about large changes, starting with a discussion with the
network/consensus team is a good idea.   Depending on the scope, you might be
asked to first write a design document or build an experiment/simulation
which illustrates the benefits (we follow the same process, e.g.  the
[_Ouroboros Leios_][leios-design] is a good example).   At this stage, there is no rule, so it's
much better to talk with the maintainers (via GitHub issue/discussion or via
email, etc.).

### Changelogs

We maintain changelogs for all our packages.

## Roles and Responsibilities

Maintainers of each package are listed in the corresponding `*.cabal` file.

We maintain a [CODEOWNERS file][CODEOWNERS] which provides information on who should
review your code if it touches given projects.  Note that you need to get
approvals from all code owners (even though GitHub doesn't give a way to
enforce it).

For a general architectural overview of the network code contact either:
@coot or @dcoutts.

For a general architectural overview of the consensus code contact either:
@dnadales or @nfrisby.

## CI

The networking code is tested both using GitHub actions on Windows and
[Hydra][hydra] on Linux & MacOS.

We officially support:

* `Linux` (`x86_64-linux`)
* `MacOS` (`x86_64-darwin` and `aarch64-darwin`)
* `Windows` (using [`msys2`] software distribution, and cross-compiled on linux with `nix`)

On 32-bit platforms, you might expect some issues (currently memory requirement
for `cardano-node` on 32 architecture are too high).

## Releasing packages to CHaP

New versions of packages are published on [CHaP].  To release packages to
[CHaP] one should use `./scritp/release-to-chap.sh`.

* First run `./script/release-to-chap.sh -r` to see which changes can be
  published.
* Update versions in `*.cabal` files according to changes in `CHANGELOG.md`
  files.
* Update `CHANGELOG.md` files.
* Run `./script/release-to-chap.sh` which will create a PR in
  `cardano-haskell-packages` repo (pointed by `CARDANO_HASKELL_PACKAGES_DIR`
  environment variable or `/tmp/chap` if it's not defined).
* Before merging that branch, run `./script/build-with-chap.sh`.  It will use the new branch in
  `cardano-haskell-packages` to restore the `ourobors-network` repository to the
  state published in `CHaP`.  One must resolve all compilation issues before
  merging the `CHaP` branch.  On a successful run, the script will add a comment
  on the `CHaP` PR.

## Release Branches

When needed we use release branches: `release/*`, but most often we just
release from `master`.

Note that `CHaP` allows us to make releases of packages independently of each
other (especially non-breaking changes), so there might be other release
branches, e.g.  `release/network-mux-*`.  They MUST follow the following
naming convention: `release/${package-name}-${version}`.

All commits in the release branch MUST be cherry-picked from `master`.  Each
time one wants to add new commits from `master`, one SHOULD:

* cherry-pick them to a new branch,
* create a PR which targets the right release branch,
* mention in the PR's description from which original PR(s) (to the `master`
  branch) the commits are coming from ([example](https://github.com/input-output-hk/ouroboros-network/pull/4120)).

This forces the changes to go through the normal pull request review process
& let CI validate the release patch set.

Note that we never merge release branches back to `master`.

## Updating dependencies (`index-state`)

Our Haskell packages come from two package repositories:
- Hackage
- [CHaP](https://github.com/input-output-hk/cardano-haskell-packages) (which is essentially another Hackage)

The `index-state` of each repository is pinned to a particular time in
`cabal.project`.  This tells Cabal to treat the repository as if it was
the specified time, ensuring reproducibility.  If you want to use a package
version from repository X which was added after the pinned index state time,
you need to bump the index state for X.  This is not a big deal, since it
changes what packages `cabal` considers to be available when doing
solving, but it will change what package versions cabal picks for the plan, and
so will likely result in significant recompilation, and potentially some
breakage.  That typically just means that we need to fix the breakage
(increasing the lower bound on the problematic package if the fix is not backward
compatible), or delay that work and instead decrease the upper-bound on the
problematic package for now.

Note that `cabal`'s persistent state includes which index states it is
aware of, so when you bump the pinned index state you may need to
call `cabal update` for `cabal` to be happy.

Whenever using a newer Hackage's index, one needs to run:
```bash
nix flake lock --update-input hackageNix
```
and when using a newer `CHaP`'s index:
```bash
nix flake lock --update-input CHaP
```

If you fail to do this you may get an error like this from Nix:
```
error: Unknown index-state 2021-08-08T00:00:00Z, the latest index-state I know about is 2021-08-06T00:00:00Z. You may need to update to a newer hackage.nix.
```

### Use of `source-repository-package`s

We *can* use Cabal's `source-repository-package` mechanism to pull in
un-released package versions. This can be useful when debugging/developing
across different repositories. However, we should not release our packages
to CHaP while we depend on a `source-repository-package` since downstream
consumers would not be able to build such a package.

If we are stuck in a situation where we need a long-running fork of a
package, we should release it to CHaP instead (see the
[CHaP README](https://github.com/input-output-hk/cardano-haskell-packages)
for more).

If you do add a temporary `source-repository-package` stanza, you need to
provide a `--sha256` comment in `cabal.project` so that Nix knows the hash
of the content. There are two relatively straightforward ways to do this:

1. The TOFU approach: put in the wrong hash and then Nix will tell you the correct one, which you can copy in.
2. Calculate the hash with `nix-shell -p nix-prefetch-git --run 'nix-prefetch-git <URL> <COMMIT_HASH>'`


[CODEOWNERS]: https://github.com/input-output-hk/ouroboros-network/blob/master/.github/CODEOWNERS
[`ghcup`]: https://www.haskell.org/ghcup/
[`QuickCheck`]: https://hackage.haskell.org/package/QuickCheck
[`io-sim`]: https://github.com/input-output-hk/io-sim
[io-sim-lib]: https://github.com/input-output-hk/io-sim/tree/main/io-sim
[cabal-with-compiler]: https://cabal.readthedocs.io/en/stable/cabal-project.html#cfg-field-with-compiler
[cicero]: https://cicero.ci.iog.io/action/current?active
[`msys2`]: https://www.msys2.org/
[leios-design]: https://iohk.io/en/research/library/papers/ouroboros-leios-design-goals-and-concepts/
[gh-signing-commits]: https://docs.github.com/en/authentication/managing-commit-signature-verification/signing-commits
[`tasty`]: https://github.com/UnkindPartition/tasty#readmy
[tasty-options]: https://github.com/UnkindPartition/tasty#options
[CHaP]: https://github.com/input-output-hk/cardano-haskell-packages
[Hackage]: https://hackage.haskell.org
[hydra]: https://github.com/NixOS/hydra
[nix-setup]: https://github.com/input-output-hk/cardano-node-wiki/blob/main/docs/getting-started/building-the-node-using-nix.md
