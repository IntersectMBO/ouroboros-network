### Building this Document

With make:

``` shell
$ make network.pdf
```

Build the document with nix:
TODO:FIX nix-build
``` shell
$ nix-shell ./default.nix --run 'make network.pdf'
```

Run `latexmk` in continuous preview mode:

``` shell
$ make watch
```
