### Building this Document

With make:

``` shell
$ make network.pdf
```

Build the document with nix:

``` shell
$ nix-shell -A network-pdf --run 'make network.pdf'
```

Run `latexmk` in continuous preview mode:

``` shell
$ make watch
```
