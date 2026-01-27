# Changelog - Kes Agent

## [v1.1.0.0]

- This version adds support for `ouroboros-network-0.24.0.0`. Older versions of `ouroboros-network` (and its associated packages) are no longer supported.

## [v1.0.0.0]

This is the first major release of the Kes Agent, intended for experimental purposes alongside `cardano-node` version 10.7. This version includes significant dependency updates, improved control handling, and enhanced deployment assets. This release is suitable for testnets, but is not currently recommended for full production use.

### 🌟 Added & Architectural Changes

* **Advanced GHC Compatibility:** The Continuous Integration (CI) process has been updated to run with a broad range of GHC versions, including `9.6.7`, `9.8.4`, `9.10.3`, and `9.12.2`, ensuring long-term compatibility.
* **Nix Flake Integration:** The project now utilizes a Nix flake configuration for reproducible builds and development environments, with support for running executables and test suites via `nix run`.
* **Documentation Hardening:** Documentation has been updated to include instructions for hardening and `cardano-node` configuration.

### 🔒 Security and Dependencies

* **Crypto Library Bump:** Updated the dependency to **`cardano-crypto-class-2.2.3.2`**, ensuring the use of the latest stable cryptographic primitives.
* **Tracing Library:** Added support for **`contra-tracer 0.1.0.2`** as an acceptable dependency, improving observability and logging capabilities.
* **`typed-protocols` Compatibility:** Updated code to be compatible with **`typed-protocols-1.1`** and improved Haddock documentation for `KESPeriod`.

### 🔧 Changed & Quality of Life

* **Improved Control Exception Handling:** Modified `kes-agent-control` and `kes-agent` to handle connection exceptions gracefully, especially on Windows, preventing verbose `HasCallStack` backtraces when the agent is down.
* **CI and Build Stability:**
    * Updated the version of Cabal used in GitHub CI to `3.14.2.0`.
    * Raised the upper version bounds for several key dependencies, including `containers` to `<0.9` and `typed-protocols`/`io-classes`.
* **Documentation Clean-up:** Fixed links in the `README.md` and removed sections from `CONTRIBUTING.md` that referenced documentation and processes exclusive to the consensus repository.

### 🩹 Fixed

* **`HasInfo` Constraints:** Removed superfluous `HasInfo` constraints from the `Serializable` instances for `SignKeyKES` and `VerKeyKES` to decouple serialization from `ouroboros-consensus`.
