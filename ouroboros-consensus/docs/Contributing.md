# How to contribute to the Consensus Layer

If you're not working for IOHK, please fork the repository on GitHub and open
PRs that way. We're excited about your involvement; thank you! If you've
recently joined IOHK, see [IOHK.md][iohk-onboarding] for additional setup.

Please read the following documents when beginning to contribute to the
Consensus Layer in this repository. A note about expectations: the rules about
style and process document our ideal scenario, but we don't expect repository
newcomers to get them right immediately; we'll guide you through them when
reviewing your PRs. We appreciate however much effort you can afford at the
start.

  * [technical onboarding][consensus-onboarding]
  * [code style guide][consensus-style]
  * [git process][consensus-git]
  * TODO quick-start guide

[consensus-git]: GitProcess.md

[consensus-onboarding]: Onboarding.md

[consensus-style]: StyleGuide.md

[iohk-onboarding]: IOHKOnboarding.md

## Profiling tips & tricks

Here there are some findings or tricks we have found useful at different times
regarding profiling the Ouroboros Consensus layer.

### Time reported in GHC `-p` profiling is off

When a Haskell program is run, the reported time on the `-p` report is the result of

    total_ticks * tick_interval / num_capabilities
    
The Cardano-node is run by default with two capabilities, this means that the
total time will be half of what one would expect.

### Time reported is lower than half of what one would expect

This can be because there are `safe` and `unsafe` calls in different parts of
the codebase.

--------------------------------------------------------------------------------
**NOTE**

To explain this we have to take some detour on FFI. The following table
shows some context about it:

|                            | `safe` | `unsafe` |
|----------------------------|--------|----------|
| Can call into Haskell code | Yes    | No       |
| Releases the capability    | Yes    | No       |
| Shows up in time profiling | No     | Yes      |

In particular, for `safe` calls, as the capability is released just before the
foreign function is called, from the Haskell point of view, said task is not
running. 

Imagine the following code in Haskell:

```haskell
foreign import ccall unsafe "foo" foo :: IO ()
foreign import ccall safe   "bar" foo :: IO ()

baz = do
  foo
  bar
```

This will be compiled to Core (`-ddump-simpl`) as:

```
...
 case {__pkg_ccall main State# RealWorld -> (# State# RealWorld #)}_d20O
                        ds_X21f
                 of
...
case {__pkg_ccall_GC main State# RealWorld -> (# State# RealWorld #)}_d20S
             ipv4_X284
      of
...
```

And to C-- (`-ddump-cmm`) as:

```
...
           call "ccall" arg hints:  []  result hints:  [] foo();
...
           (_u3jD::I64) = call "ccall" arg hints:  [PtrHint,]  result hints:  [PtrHint] suspendThread(BaseReg, 0);
           call "ccall" arg hints:  []  result hints:  [] bar();
           (_u3jE::I64) = call "ccall" arg hints:  [PtrHint]  result hints:  [PtrHint] resumeThread(_u3jD::I64);
...
```

This shows how a `safe` call indeed releases the capability. Imagining a super
simplified scenario with initially one OS thread, one Haskell task/thread and
only one capability that performs the `baz` call:

```
= has the capability
- does not have the capability

           v call to `foo`      v call to `bar`
...===============================-------------------========= ...
                              ^ return             ^ return
```

So in the above, the time during the `safe` call, the Haskell capability is
`IDLE` thus not counting for the time spent. What happens if we have more
Haskell tasks waiting to be executed (note this will require multiple OS
threads)?

```
= has the capability
- does not have the capability

         v call to `foo`      v call to `bar`
...===============================----------------------========= ...
                              ^ return             ^ return
...-----------------------------========================-------- ...
```

Note now that the foreign call might finish before the capability is available
again, and in this case it has to wait. 

From Haskell's point of view, no waiting time happened in this last scenario as
the capability was all the time running Haskell code. In case no Haskell tasks
can be run in that time when the C code is being executed, then said time will
not appear in the profiling report as _no Haskell code is being executed_ (a
case similar to the first diagram).

--------------------------------------------------------------------------------

Now, the reported time on the Haskell time profiling will be off due to these
`safe` foreign calls not being accounted for. Apart from that, `unsafe` calls
sometimes lose ticks when running with the default tick interval. Therefore it
is advised to run with `-V0.01` when doing time measurements.

### Time profiling and eventlog

One of the few approaches that seemed useful for investigating time consumption
was using [hs-speedscope](https://github.com/mpickering/hs-speedscope) to graph
all the execution time registered by Haskell, and then combining it with
[ghc-events-analyze](https://github.com/well-typed/ghc-events-analyze) to see
time elapsed between different events that we care of.

In order to emit time profiling on the eventlog, the executable has to be linked
with `-eventlog` to pull the right RTS and then run with `-l`. If the eventlog
produced is too big, one can run with `-l-au` so that (`-a`) all events are
omitted, (`u`) except the user events. Also one can set the output name for the
eventlog file with `-ol<name>`. An example from `cardano-node`:

```bash
> cat cardano-node.cabal
...
executable cardano-node
...
    ghc-options:        -eventlog
                        "-with-rtsopts=-T -I0 -A16m -N2 --disable-delayed-os-memory-return"
...
> cat cabal.project.local
...
profiling: true
profiling-detail: all-functions

package plutus-core
  ghc-options: -fexternal-interpreter
...
> cabal run exe:cardano-node -- run +RTS -l-au -p -olname.eventlog -RTS
...
> ls
...
name.eventlog
```

It is important to note too that `-fno-prof-count-entries` can make the
concurrent profiled code run significantly faster (see
[this](https://downloads.haskell.org/ghc/latest/docs/html/users_guide/profiling.html#profiling-parallel-and-concurrent-programs)).

```
> cabal run --ghc-options="-fno-prof-count-entries" <exe> -- <args> +RTS -l-au -p -olname.eventlog -RTS
```

### External interpreter (`ByteCodeLink.lookupCE`)

Template haskell has some strange interaction with profiling, which was reported
to the GHC team at [this
ticket](https://gitlab.haskell.org/ghc/ghc/-/issues/18320). The solution is
simple: use `-fexternal-interpreter` for the packages that fail with that error.

However it is important to note that at least in our case, we cannot build the whole project with `-fexternal-interpreter` as there is an error related to `malloc`ing memory:

```
> cabal build --ghc-options="-fexternal-interpreter" ...
...
[45 of 63] Compiling Cardano.Node.Tracing.Tracers.ChainDB ( src/Cardano/Node/Tracing/Tracers/ChainDB.hs, .../dist-newstyle/build/x86_64-linux/ghc-8.10.4/cardano-node-1.33.0/build/Cardano/Node/Tracing/Tracers/ChainDB.o, .../dist-newstyle/build/x86_64-linux/ghc-8.10.4/cardano-node-1.33.0/build/Cardano/Node/Tracing/Tracers/ChainDB.dyn_o ) [Data.Aeson.KeyMap changed]
ghc-iserv: mmap 131072 bytes at (nil): Cannot allocate memory
ghc-iserv: Try specifying an address with +RTS -xm<addr> -RTS
ghc-iserv: internal error: m32_allocator_init: Failed to map
    (GHC version 8.10.4 for x86_64_unknown_linux)
    Please report this as a GHC bug:  https://www.haskell.org/ghc/reportabug
ghc: ghc-iserv terminated (-6)
cabal: Failed to build cardano-node-1.33.0 (which is required by
exe:cardano-node from cardano-node-1.33.0).
```

So the solution is as shown in the example above, use the external interpreter
only on `plutus-core` which is the package that currently fails:

```
> cat cabal.project.local
...
profiling: true
profiling-detail: all-functions

package plutus-core
  ghc-options: -fexternal-interpreter
...
```
