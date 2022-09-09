# Consensus tests

This package contains:

* `src`: infrastructure for running consensus-related tests.

* `test-consensus`: Tests of the various consensus components which are
  unrelated to storage.

* `test-storage`: Tests of the storage layer.

* `test-infra`: Tests of some of the infrastructure in `src`.

* `test-examples` : Tutorial Introduction to the major consensus classes, 
  as compilable Haskell code. Currently, `main` is trivial (future work), but
  see these files:
  ```
  test-examples/Test/Tutorial.lhs
  test-examples/Test/Tutorial2.lhs
  ```
  
  These files are written in a `markdown+lhs` style understood by `pandoc`.
  To generate a documents from them file - it is as simple as:
    ```
    pandoc -s -f markdown+lhs test-examples/Test/Tutorial.lhs -o <output file>
    ```
  Which will work for many of the the output types supported by `pandoc`.
