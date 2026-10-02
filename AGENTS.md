# Ouroboros Network

Ouroboros Network implements network protocols and applications for the Cardano blockchain:
- **network-mux**: Multiplexing library
- **ouroboros-network**: Networking layer for the Ouroboros blockchain protocol
- **cardano-diffusion**: Networking layer for the Cardano blockchain protocol
- **cardano-ping**: Utility for pinging Cardano nodes
- **ntp-client**: NTP client
- **monoidal-synchronisation**: Monoidal synchronisation primitives
- **acts-generic**: Generic instances for the `Act` type class
- **quickcheck-monoids**: QuickCheck utilities for monoids

See [CONTRIBUTING.md](CONTRIBUTING.md) for build setup, coding standards, style guides, and contribution workflow.

## Build

```bash
cabal build all
```

Note: `cabal build` (without `all`) fails at the repo root since there is no package there.

## Testing

See CONTRIBUTING.md for test commands and patterns. Example:

```bash
cabal run ouroboros-network:ouroboros-network-sim-tests -- -p "TxSubmission"
```

## Code Formatting

The script runs `stylish-haskell` in-place and then shows a `git diff` of what changed. Only run it on files relevant to your current work — reformatting unrelated files creates noise in your diff.

```bash
./scripts/ci/run-stylish-haskell.sh -u   # uncommitted changes only
./scripts/ci/run-stylish-haskell.sh -c   # files in HEAD only
```

Do not run the script without flags — that reformats all `.hs` files in the repo.

Config: `.stylish-haskell-network.yaml`. Requires `fd`/`fdfind` and `stylish-haskell`.

## Releasing

Follow "Releasing packages to CHaP" in CONTRIBUTING.md.  The description of a
release PR must list the PRs included in the release, grouped by package; this
list is used to update the [release board].  For each released package, find
the PRs merged since its previous release tag:

```bash
git log --first-parent --oneline <package>-<previous-version>..HEAD -- <package>
```

Merge commit subjects only contain the PR number (`Merge pull request #NNNN
from ...`); get each title with `gh pr view NNNN --json title -q .title`, and
list every PR as `#NNNN <title>`.

[release board]: https://github.com/orgs/IntersectMBO/projects/5/views/18

## Important Notes

- When using si-timers from io-classes; **`threadDelay`, `timeout`, `registerDelay` take `DiffTime` (seconds), not microseconds.** Using `3_000_000` means three months, not three seconds.

## Key Architectural Patterns

### Typed Protocols

This codebase uses [typed-protocols](https://github.com/input-output-hk/typed-protocols) for protocol state machines. When modifying protocols:
1. Update the protocol type definition first
2. Ensure all state transitions are covered
3. Update `Codec.hs` for CBOR serialization
4. Add tests in the corresponding test module

### STM-based Coordination

Heavy use of `StrictTVar` from `Control.Concurrent.Class.MonadSTM.Strict`. When modifying shared state:
- Keep critical sections minimal

### Tracing

All components use `Control.Tracer`. When adding new trace events:
1. Add constructor to the relevant `Trace*` type
2. Consider performance impact (avoid expensive operations in hot paths)
