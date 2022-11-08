# Contributing to Ouroboros-Network

## Coverage Reports

### Generating Coverage Reports

A haskell.nix project with coverage enabled for all project packages is exposed under the `coveredProject` attribute of `default.nix`:

```
nix-build default.nix -A coveredProject
```

A coverage report for the entire project can be generated and viewed with:

```
nix-build default.nix -A coveredProject.projectCoverageReport
xdg-open ./result/share/hpc/vanilla/html/index.html
```

Coverage reports for each individual package can be generated with:

```
nix-build default.nix -A coveredProject.hsPkgs.$pkg.coverageReport
xdg-open ./result/share/hpc/vanilla/html/index.html
```

Although individual package reports are also available in the entire project coverage report.

### Debugging Coverage Reports

The Nix derivation used to generate coverage reports can be debugged with:

```
nix-shell default.nix -A coveredProject.projectCoverageReport
OR nix-shell default.nix -A coveredProject.hsPkgs.$pkg.coverageReport
cd $(mktemp -d)
out=$(pwd) genericBuild
```

Build logs are written to stdout and artifacts written to the current directory.

## Releasing packages to CHaP

When new versions of the packages are released, they should be included in [CHaP](https://github.com/input-output-hk/cardano-haskell-packages).
See the CHaP README for [instructions](https://github.com/input-output-hk/cardano-haskell-packages#-from-github).

## Updating dependencies

Our Haskell packages come from two package repositories:
- Hackage
- [CHaP](https://github.com/input-output-hk/cardano-haskell-packages) (which is essentially another Hackage)

The "index state" of each repository is pinned to a particular time in
`cabal.project`.  This tells Cabal to treat the repository as if it was
the specified time, ensuring reproducibility.  If you want to use a package
version from repository X which was added after the pinned index state
time, you need to bump the index state for X.  This is not a big deal,
since all it does is change what packages `cabal` considers to be available
when doing solving, but it will change what package versions cabal picks
for the plan, and so will likely result in significant recompilation, and
potentially some breakage.  That typically just means that we need to fix
the breakage (increasing the lower-bound on the problematic package if fix 
is not backward compatible), or delay that work and instead decrease the 
upper-bound on the problematic package for now.

Note that `cabal`'s own persistent state includes which index states it is 
aware of, so when you bump the pinned index state you may need to
call `cabal update` in order for `cabal` to be happy.

The Nix code which builds our packages also needs some information relating 
to the index-state. This information needs to be new enough to include 
the index-state specified in `cabal.project`. The information is represented 
by inputs managed by `niv`:
You can update these by running:
- `niv update hackage.nix` for Hackage
- `niv update CHaP` for CHaP

If you fail to do this you may get an error like this from Nix:
```
error: Unknown index-state 2021-08-08T00:00:00Z, the latest index-state I know about is 2021-08-06T00:00:00Z. You may need to update to a newer hackage.nix.
```

### Use of `source-repository-package`s

We *can* use Cabal's `source-repository-package` mechanism to pull in
un-released package versions. This can be useful when debugging/developing
across different repositories. However, we should not release our packages
to CHaP while we depend on a `source-repository-package` since downstream
consumers would not be able to build such package.

If we are stuck in a situation where we need a long-running fork of a
package, we should release it to CHaP instead (see the
https://github.com/input-output-hk/cardano-haskell-packages[CHaP README]
for more).

If you do add a temporary `source-repository-package` stanza, you need to
provide a `--sha256` comment in `cabal.project` so that Nix knows the hash
of the content. There are two relatively straightforward ways to do this:

1. The TOFU approach: put in the wrong hash and then Nix will tell you the correct one, which you can copy in.
2. Calculate the hash with `nix-shell -p nix-prefetch-git --run 'nix-prefetch-git <URL> <COMMIT_HASH>'`
