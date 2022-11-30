```mermaid
graph TB
  classDef df fill:#E57A77;
  classDef tf fill:#7CA1CC;
  classDef kind fill:#A8B6CC;

  Concept[Concept or kind]; class Concept kind;
  DF[Data family]; class DF df;
  TF[Type family]; class TF tf;
```

```mermaid
flowchart LR;

  classDef df fill:#E57A77;
  classDef tf fill:#7CA1CC;
  classDef kind fill:#A8B6CC;

  Block(Block)-->BlockProtocol[BlockProtocol blk]; class Block kind;
  BlockProtocol[BlockProtocol blk]-.->Protocol(Protocol); class BlockProtocol tf;
  Block-->BlockConfig[BlockConfig blk]; class BlockConfig df
  Block-->StorageConfig; class StorageConfig df
  Block-->CodecConfig; class CodecConfig df
  Block-->Header; class Header df
  Ledger(Ledger); class Ledger kind
  Block-->LedgerState[LedgerState blk]; class LedgerState df;

  LedgerState-.->Ledger
  Ledger-->AuxLedgerEvent[AuxLedgerEvent l]; class AuxLedgerEvent tf
  Ledger-->LedgerErr[LedgerErr l]; class LedgerErr tf

  Protocol(Protocol)-->ConsensusConfig[ConsensusConfig p]
  subgraph ConsensusProtocol[class ConsensusProtocol]
    ChainDepState[ChainDepState p]; class ChainDepState tf
    IsLeader[IsLeader p]; class IsLeader tf
    CanBeLeader[CanBeLeader p]; class CanBeLeader tf
    SelectView[SelectView p]; class SelectView tf
    LedgerView[LedgerView p]; class LedgerView tf
    ValidationErr[ValidationErr p]; class ValidationErr tf
    ValidateView[ValidateView p]; class ValidateView tf
  end
  Protocol-->ChainDepState
  Protocol-->IsLeader[IsLeader p]
  Protocol-->CanBeLeader[CanBeLeader p]
  Protocol-->SelectView[SelectView p]
  Protocol-->LedgerView[LedgerView p]
  Protocol-->ValidationErr[ValidationErr p]
  Protocol-->ValidateView[ValidateView p]

```
