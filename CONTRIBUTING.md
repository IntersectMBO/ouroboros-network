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
