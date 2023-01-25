This document is a high-level decomposition of the remaining work for Genesis.

The parentheticals in some of the nodes are just clarifying notes to ourselves, not meant for more general audiences to interpret.

This top-level DAG captures all the of the remaining work.
Every node is a task (of varying difficulty).
An arrow from X to Y means that we cannot complete Y until we have completed X.
We typically can start Y before we finish X.

The circles will be expanded into other diagrams below; this document does not further decompose the rectangles.

This document makes no claims about time.
In particular, these phases are likely very assymetrical, so please do not assume they correspond to any unit of time.

```mermaid
graph TD
    Review1["review with Researchers&Architects for the 1st time"]
    style Review1 fill:#776633
    Review2["review with Researchers&Architects for the 2nd time"]
    style Review2 fill:#776633

    EngineeringPhase1((1st engineering phase))
    EngineeringPhase2((2nd engineering phase))
    EngineeringPhase3((3rd engineering phase))
    EngineeringPhase1 --> Review1 --> EngineeringPhase2 --> Review2 --> EngineeringPhase3 --> MVP["announce MVP"]

    PlanQA["review with SDET team\n& negotiate with Product Owners"]
    style PlanQA fill:#667733
    EngineeringPhase2 --> SysLevelTests["specify system level\nvalidation tests"]
    EngineeringPhase2 --> SysLevelBench["specify system level\nperformance goals\n(typical and also during attack)"]
    SysLevelTests --> PlanQA
    SysLevelBench --> PlanQA

    SyncNetworkTeam["review with Network Team"]
    style SyncNetworkTeam fill:#773366
    EngineeringPhase2 --> BootStrapDistr["final assumptions\nwrt bootstrap stake dist"] --> SyncNetworkTeam
    EngineeringPhase2 --> LedgerPeers["final assumptions\nwrt ledger peers"]   --> SyncNetworkTeam
```

## 1st Engineering Phase

We need to finish our design, justification for it, incl. tuned prototypes.
Then we can present it to the Researchers and Architects for criticism.

```mermaid
graph TD
    ProtoDR["prototype the Disconnect Rule\n(incl. density comparisons and LoE)"]
    ProtoCSJ["prototype the ChainSync Jumping,\nexcl. slow path"]
    ProtoLoP["prototype the ChainSync Client\n(incl. LoP and LoR)"]

    ProtoCSJ --> TuningCSJSync[tune CSJ for fast path sync times]
    ProtoDR  --> OptimizingDR[optimize DR]
    ProtoLoP --> TuneLoP[tune CSC]
    
    ProtoCSJ --> DocCSJ["document CSJ,\nexcl. slow path\n(incl. justification)"]
    ProtoDR  --> DocDR["document DR\n(incl. justification)"]
    ProtoLoP --> DocLoP["document CSC\n(incl. justification)"]

    DocDR  --> TestDR{"smoke test\nDR"}
    DocCSJ --> TestCSJ{"smoke test\nfast CSJ"}
    DocLoP --> TestLoP{"smoke test\nCSC"}
    style TestDR fill:#c27832
    style TestCSJ fill:#c27832
    style TestLoP fill:#c27832
```

## 2nd Engineering Phase

We expect the first review to yield some alterations, which we now incorporate into the prototypes and the tests

```mermaid
graph TD
    ProtoDR2["update the DR prototype"]
    ProtoCSJ2["update the CSJ prototype,\nnow incl. slow path"]
    ProtoLoP2["update the CSC prototype"]

    ProtoCSJ2 --> DocCSJ2["document updated DR\n(incl. justification)"]
    ProtoDR2  --> DocDR2["document updated CSJ\n(incl. justification)"]
    ProtoLoP2 --> DocLoP2["document updated CSC\n(incl. justification)"]

    DocDR2  --> TestDR2{test DR}
    DocCSJ2 --> TestCSJ2{"test CSJ\n(incl. both fast and slow)"}
    DocLoP2 --> TestLoP2{test CSC}
    style TestDR2  fill:#c27832
    style TestCSJ2 fill:#c27832
    style TestLoP2 fill:#c27832
```

## 3rd Engineering Phase

Finally, after testing the final prototypes, we can refine them into implementations, which together yield the MVP.

```mermaid
graph TD
    MvpDR["slightly refine DR\nprototype into implementation"]
    MvpLoP["slightly refine CSC\nprototype into implementation"]
    MvpCSJ["slightly refine CSJ\nprototype into implementation"]
```
