# network-mux

Multiplexing library. It allows to run multiple network applications over
a single bearer. The multiplexer cuts messages in chunks of some maximal size
and sends them over a bearer channel. The current version of this library
relies on reliable and ordered delivery of messages. The multiplexer should run
alongside an incremental decoder.

Example protocol with an incremental
decoder is implemented in
[Test.Mux.ReqResp](https://github.com/intersectmbo/ouroboros-network/blob/master/network-mux/test/Test/Mux/ReqResp.hs)
for other examples of protocols see 'typed-protocols' or 'ouroboros-network'
packages.

## tests

To run the test suite:
```
cabal new-run test-network-mux
```
or
```
nix-build -A haskellPackages.network-mux.checks
```

## egress-bucket demo

`mux-bucket-demo` measures the egress token bucket (`Network.Mux.Egress.Bucket`)
on real TCP, in the shape of the Leios ignition experiment: X peers each fetch
one EB from a server at the same instant.

    cabal build network-mux:exe:mux-bucket-demo
    D=$(cabal list-bin network-mux:exe:mux-bucket-demo)
    $D server --port 6000 --peers 200 --eb-mb 12 --chunk-kb 197 \
              --budget-mbps 950 --lowat 131072 --order fifo --horizon 150
    $D client --host 127.0.0.1 --port 6000 --peers 200 --horizon 150

Server: `--budget-mbps 0` runs the unscheduled mux; `--lowat` sets
`TCP_NOTSENT_LOWAT` (bytes; Linux and macOS); `--order fifo` is an equal
per-batch share, `--order arrival` strict priority by connection order;
`--chunk-kb 0` serves the EB as one message.  Client: `--stall K` makes the
first K peers raw sockets that never read (`--stall-read-bps` for a slow
reader).  The client prints completion quantiles, the count inside 7 s and the
aggregate rate; the server prints whether LOWAT took effect, the grant count
with the time waited for writability and for tokens, and one line per
connection that did not complete.

`demo/mux-bucket-demo.sh` runs the standard matrix — kernel baseline, equal
and strict bucket, budget below the link, stalled peers with and without
LOWAT, and a 66 ms emulated RTT — keeping both outputs per run and printing
the results as two tables (`… OUTDIR report NAME…` re-prints them from saved
outputs).  The runs that emulate a link use `sudo tc` on `lo`, which affects
all loopback traffic while they last; `TC=0` skips them.  On two hosts no
`tc` is needed.
