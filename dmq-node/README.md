# dmq-node

## Purpose

`dmq-node` is a small executable in this repository used to run a DMQ (distributed message queue) node for development, testing, and experimentation. It exposes the local node implementation and command-line entrypoint useful for debugging DMQ behavior and exercising the networking code in isolation.

## Location

The package and sources live in the `dmq-node` directory of this repository. Use the project-level `cabal` setup to build and run it.

## Prerequisites

- GHC and `cabal-install` compatible with this repository (see `cabal.project`).
- A POSIX-compatible shell is helpful for some examples; Windows users can use WSL or PowerShell as appropriate.

## Build

From the repository root, you can build the package with `cabal`:

```bash
cabal build dmq-node
```

This will compile the `dmq-node` executable. If you prefer to run without a separate build step, use `cabal run` (next section).

## Run

To see the available command-line options, run:

```bash
cabal run dmq-node -- --help
```

To launch the node (example invocation):

```bash
cabal run dmq-node -- --help
# or, after building
cabal run dmq-node -- <options>
```

Replace `<options>` with command-line flags printed by `--help`. The flags and configuration change over time; consult `--help` for the most accurate information.

If you prefer to run the compiled binary directly, find it under `dist-newstyle` after building and execute it with the same flags.

## Typical usage and tips

- Use `--help` to discover available options and configuration files the executable accepts.
- When experimenting locally, run multiple instances with different ports/addresses to simulate peers.
- Capture logs (redirect stdout/stderr) when reproducing issues; include them in bug reports.

## Development and debugging

- The source code for the executable is in this directory — inspect the `src/` subdirectory to find the `main` entry point and CLI parsing.
- When making changes, run `cabal build` and then exercise the node with `cabal run` to validate behavior.

## Reporting issues

If you encounter problems running `dmq-node` or believe the README is missing important information, please open an issue on the repository describing the steps to reproduce and include `--help` output and relevant logs.

---

If you need a more detailed walkthrough (examples of common invocations, sample configuration files, or automated test harnesses), open an issue or a pull request adding that material.
