# Major Type and Data Families in Consensus

I started with

```
$ git show
commit bf8579cc2ff2a7bc4ba23150eff659cfd1c6ccca (origin/master)
[snip]
Date:   Tue Jan 12 14:08:36 2021 +0000

    Merge #2855
[snip]
$ git status -suno
$ git grep -E \
    -e '^(data|type) +family' \
    -e '^ +(data|type)[^=]*$' \
    origin/master -- 'ouroboros-consensus/src/*hs'
```

and then snipped out what seemed too incidental, too general, too obvious, and
so on.

I've also rearranged the order of the results and added a few tightly coupled
declarations/definitions of type synonyms and data types.

## Indicies

The primary type families are indexed by the block type (see `blk` below), the
protocol type (see `p` below; it's an empty type, it's just a tag), the ledger
state type (see `l` below, or `st` or `lst` etc), and the transaction type (see
`tx` below). If the block type is fixed, then so is (TODO nearly?) every other
type: it's the most comprehensive type.

## Config

All of the Consensus configuration data required for a specific block type.
This is a required argument to the primary Consensus entrypoint.

```
Ouroboros/Consensus/Config.hs:data TopLevelConfig blk = TopLevelConfig {
Ouroboros/Consensus/Config.hs-      topLevelConfigProtocol :: !(ConsensusConfig (BlockProtocol blk))
Ouroboros/Consensus/Config.hs-    , topLevelConfigLedger   :: !(LedgerConfig blk)
Ouroboros/Consensus/Config.hs-    , topLevelConfigBlock    :: !(BlockConfig blk)
Ouroboros/Consensus/Config.hs-    , topLevelConfigCodec    :: !(CodecConfig blk)
Ouroboros/Consensus/Config.hs-    , topLevelConfigStorage  :: !(StorageConfig blk)
Ouroboros/Consensus/Config.hs-    }

Ouroboros/Consensus/Protocol/Abstract.hs:data family ConsensusConfig p :: Type

Ouroboros/Consensus/Block/Abstract.hs:data family BlockConfig blk :: Type
Ouroboros/Consensus/Block/Abstract.hs:data family CodecConfig blk :: Type
Ouroboros/Consensus/Block/Abstract.hs:data family StorageConfig blk :: Type

Ouroboros/Consensus/Ledger/Basics.hs:type LedgerConfig      blk = LedgerCfg (LedgerState blk)
Ouroboros/Consensus/Ledger/Basics.hs:type family LedgerCfg l :: Type

type LedgerConfig      blk = LedgerCfg (LedgerState blk)
```

## Consensus _______? TODO

Each block type implies a single protocol. This is even true for the
`HardForkBlock` block that composes blocks; it maps to the data type
`HardForkProtocol`.

The `HardForkProtocol` type also composes blocks instead of composing
protocols, as might have been expected. The HFC interface ultimately requires
and respects a mapping from the listed block types to their respective
protocols; cf `AcrossEraSelection`.

```
Ouroboros/Consensus/Block/Abstract.hs:type family BlockProtocol blk :: Type
```

Each block type implies a corresponding block header type.

```
Ouroboros/Consensus/Block/Abstract.hs:data family Header blk :: Type
```

## Block Forge

```
Ouroboros/Consensus/Block/Forging.hs:type family CannotForge blk :: Type
Ouroboros/Consensus/Block/Forging.hs:type family ForgeStateInfo blk :: Type
Ouroboros/Consensus/Block/Forging.hs:type family ForgeStateUpdateError blk :: Type
```

## HFC

The codomain of `HardForkIndices` is a list of block types.

```
Ouroboros/Consensus/HardFork/Abstract.hs:  type family HardForkIndices blk :: [Type]
```

The HFC combinator automates the conversion between wallclock times and slot
numbers. A `PartialFoo` is `Foo` less the portion of `Foo` that corresponds to
those time conversions. Given a `PartialFoo`, the HFC then continually provides
the complete `Foo` that contains the most complete/recent time conversion data.

```
Ouroboros/Consensus/HardFork/Combinator/PartialConfig.hs:  type PartialConsensusConfig p :: Type
Ouroboros/Consensus/HardFork/Combinator/PartialConfig.hs:  type PartialLedgerConfig blk :: Type
```

## Header Validation 

Header validation may require some addition info, depending on the era eg; that
is the `TipInfo`.

```
Ouroboros/Consensus/HeaderValidation.hs:  type TipInfo blk :: Type
```

## Ledger

```
type LedgerError       blk = LedgerErr (LedgerState blk)
type TickedLedgerState blk = Ticked    (LedgerState blk)

Ouroboros/Consensus/Ledger/Basics.hs:data family LedgerState blk :: Type
Ouroboros/Consensus/Ledger/Basics.hs:  type family LedgerErr l :: Type

Ouroboros/Consensus/Ledger/Query.hs:data family Query blk :: Type -> Type
Ouroboros/Consensus/Ledger/Inspect.hs:  type LedgerWarning blk :: Type
Ouroboros/Consensus/Ledger/Inspect.hs:  type LedgerUpdate  blk :: Type

Ouroboros/Consensus/Ledger/SupportsMempool.hs:data family GenTx blk :: Type
Ouroboros/Consensus/Ledger/SupportsMempool.hs:type family ApplyTxErr blk :: Type
Ouroboros/Consensus/Ledger/SupportsMempool.hs:data family TxId tx :: Type
```

## Network Versions

```
Ouroboros/Consensus/Node/NetworkProtocolVersion.hs:  type BlockNodeToNodeVersion   blk :: Type
Ouroboros/Consensus/Node/NetworkProtocolVersion.hs:  type BlockNodeToClientVersion blk :: Type
```

## Protocol

```
Ouroboros/Consensus/Protocol/Abstract.hs:  type family ChainDepState p :: Type
Ouroboros/Consensus/Protocol/Abstract.hs:  type family IsLeader p :: Type
Ouroboros/Consensus/Protocol/Abstract.hs:  type family CanBeLeader p :: Type
Ouroboros/Consensus/Protocol/Abstract.hs:  type family SelectView p :: Type
Ouroboros/Consensus/Protocol/Abstract.hs:  type family LedgerView p :: Type
Ouroboros/Consensus/Protocol/Abstract.hs:  type family ValidationErr p :: Type
Ouroboros/Consensus/Protocol/Abstract.hs:  type family ValidateView p :: Type
```

## The Concept of Aging

When we advance the state resulting from some transition through time, we might
carry along contextual information. Note that advancing the state through time
may change the values in it (eg if parts of the state correspond to scheduled
events, those might "happen" during advancement through time). For example,
some extra information about the last transition (ie other than merely "time
passing") that resulted in this state before it was advanced through time.

Named after the notion of a _clock tick_.

```
Ouroboros/Consensus/Ticked.hs:data family Ticked st :: Type
```
