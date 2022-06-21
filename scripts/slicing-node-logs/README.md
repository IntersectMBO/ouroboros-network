This directory contains scripts useful for a detailed analysis of node logs, particularly those resulting from a Benchmarking run.

* `prelude.jq`  (add this to `$HOME/.jq` on your machine)
* `slice.sh`
* `binning.awk`
* `cumsum1.awk`

If any such analysis becomes crucial, we should hardcode it in Haskell. But for data exploration, these scripts are helpful.

For example, presuming you have access to the Benchmarking Team's head node, you can run the following command to see the archived logs.

```
$ ls -ltrF ~/bench-1/runs/ | tail -n 5
drwxr-xr-x  59 dev users 12288 Jun  2 15:49 2022-06-01-18.53.1.35.0.9823.k51-5ep-360kTx-4000kU-1000kD-64kbs/
drwxr-xr-x  59 dev users 12288 Jun  7 00:38 2022-06-03-01.53.1.35.0.9823.k51-7ep-14kTx-4000kU-1000kD-73kbs-12MUTx-10BStTx-50MUBk-40BStBk-1i-1o--null/
drwxr-xr-x  59 dev users 12288 Jun  8 08:19 2022-06-07-08.50.1.35.0-rc2.95c3.k51-5ep-360kTx-4000kU-1000kD-64kbs/
drwxr-xr-x   5 dev users 61440 Jun  9 12:20 deploy-logs/
drwxr-xr-x   4 dev users  4096 Jun  9 12:51 2022-06-09-12.49.1.35.0-rc2.95c3.k51-7ep-14kTx-4000kU-1000kD-73kbs-12MUTx-10BStTx-50MUBk-40BStBk-1i-1o--null/
```

We could collate some per-block data on one of those runs as follows.

```
$ cd tmp/slicing-demo
$ run=bench-1/runs/2022-06-07-08.50.1.35.0-rc2.95c3.k51-5ep-360kTx-4000kU-1000kD-64kbs
$ . slice.sh
0 Mon 13 Jun 2022 11:35:24 PM UTC
1 Mon 13 Jun 2022 11:35:27 PM UTC
2 Mon 13 Jun 2022 11:35:31 PM UTC
[*snip*]
49 Mon 13 Jun 2022 11:38:19 PM UTC
50 Mon 13 Jun 2022 11:38:23 PM UTC
51 Mon 13 Jun 2022 11:38:27 PM UTC
{"length":20,"ns":["cardano.node.basicInfo.slotLengthByron"]}
{"length":100,"ns":["cardano.node.basicInfo.epochLengthByron"]}
{"length":1,"ns":["cardano.node.basicInfo.slotLengthShelley"]}
{"length":8000,"ns":["cardano.node.basicInfo.epochLengthShelley"]}
{"length":1,"ns":["cardano.node.basicInfo.slotLengthAllegra"]}
{"length":8000,"ns":["cardano.node.basicInfo.epochLengthAllegra"]}
{"length":1,"ns":["cardano.node.basicInfo.slotLengthMary"]}
{"length":8000,"ns":["cardano.node.basicInfo.epochLengthMary"]}
{"length":1,"ns":["cardano.node.basicInfo.slotLengthAlonzo"]}
{"length":8000,"ns":["cardano.node.basicInfo.epochLengthAlonzo"]}
{"length":1,"ns":["cardano.node.basicInfo.slotLengthBabbage"]}
{"length":8000,"ns":["cardano.node.basicInfo.epochLengthBabbage"]}
unsetting run=bench-1/runs/2022-06-07-08.50.1.35.0-rc2.95c3.k51-5ep-360kTx-4000kU-1000kD-64kbs
```

The script reports the epoch lengths, which can be a good touchpoint for assessing what kind of nodes were running.
We source `slice.sh` instead of running it so that it can unset `run`, so as to prevent the user from getting confused about which run they're analyzing/reading from.
(Added based on my personal experience.)

That `slice.sh` invocation creates many JSON files, each containing data specific to a particular kind of event.

```
$ ls -ltrF
total 84
lrwxrwxrwx 1 dev users    14 Jun 13 22:54 binning.awk -> ../binning.awk
lrwxrwxrwx 1 dev users    14 Jun 13 22:54 cumsum1.awk -> ../cumsum1.awk
-rw-r--r-- 1 dev users    81 Jun 13 22:55 run
drwxr-xr-x 2 dev users  4096 Jun 13 23:04 forged/
drwxr-xr-x 2 dev users  4096 Jun 13 23:05 check/
drwxr-xr-x 2 dev users  4096 Jun 13 23:05 announce/
drwxr-xr-x 2 dev users  4096 Jun 13 23:05 wholeselected/
-rw-r--r-- 1 dev users  2730 Jun 13 23:34 slice.sh
-rw-r--r-- 1 dev users 17041 Jun 13 23:38 slots.json
-rw-r--r-- 1 dev users 13083 Jun 13 23:38 onset.json
-rw-r--r-- 1 dev users 12770 Jun 13 23:38 check.json
-rw-r--r-- 1 dev users 12203 Jun 13 23:38 isleader.json
$ find . -name 'node-13.json'
./announce/node-13.json
./check/node-13.json
./forged/node-13.json
./wholeselected/node-13.json
$
$ head */README
==> announce/README <==
TraceChainSyncServerUpdate rising edge

==> check/README <==
TraceStartLeadershipCheck to TraceForgedBlock

==> forged/README <==
TraceNodeIsLeader to TraceForgedBlock

==> wholeselected/README <==
PoppedBlockFromQueue to AddedToCurrentChain
```

For now, see `slice.sh` itself for more details, including patterns to copy if you're interested in events it doesn't yet track.

In particular, the bottom of `slice.sh` lists one command I used for investigating forge times.

```
$ for i in $(seq 0 51); do
>   { cat forged/node-$i.json | jq -c '. + {forged: true}'; cat announce/node-$i.json; } | \
>     jq -cs 'group_by(.hash) | .[] | select(5 < length) | select(first | .forged) | (p(.[1] | .at) - p(first | .at))'
> done | \
>   sort -n | awk -f binning.awk -v sz=10 | uniq -c | awk -f cumsum1.awk
2110 0
2774 10
2831 20
2854 30
2857 40
2860 50
2861 70
2863 80
2874 100
2876 200
2879 1000
2880 2000
2882 4000
2883 6000
2884 10000
```

That's a cumulative histogram with exponentially growing bins.
The second column is the milliseconds between the block being `forged` and the first time its forging node `announce`d it.
Do note, however, that the log files only record centiseconds.
For example, only 2884 - 2863 = 21 blocks took more than 99 milliseconds to announce.
2110 took less than 10 milliseconds to announce.

That command works by concatenating the forge events and the announce events, done by all nodes.
We label the forging events, so we can distinguish them later.
Then it groups all those events by hash and discards the blocks that have less than 5 events; in Benchmarking runs most node have more peers than that.
Since we concatenated forge events first, and `group_by` is stable, the first event in the array will be forging event iff this node forged the block.
Then we subtract the timestamp of the forge event from the timestamp of the first announce event.
It's quite dense, but that can be helpful when exploring how to analyse this amount of data for some low-level investigation.

Another example is the time between a block being forged and the first time it was selected, which might even be on another node (as of pipelining).

```
$ { cat forged/node-*.json | jq '{at, hash, forged: true}'; cat wholeselected/node-*.json; } | \
>   jq -cs 'group_by(.hash) | .[] |  select(first | .forged) | select(1 < length) | ((.[1:] | map(p(.at)) | min) - p(first | .at))' | \
>   sort -n | awk -f binning.awk -v sz=10 | uniq -c | awk -f cumsum1.awk
56 0
320 10
587 20
913 30
1057 40
1501 50
1918 60
2130 70
2261 80
2420 90
2822 100
2825 200
2826 300
2828 400
2831 700
2832 900
2860 1000
2862 2000
2863 3000
2864 4000
2867 5000
2868 7000
2869 8000
2870 10000
```
