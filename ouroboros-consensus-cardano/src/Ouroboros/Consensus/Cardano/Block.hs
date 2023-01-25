{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE TypeFamilyDependencies   #-}
{-# LANGUAGE TypeOperators            #-}
{-# LANGUAGE ViewPatterns             #-}
module Ouroboros.Consensus.Cardano.Block (
    -- * Eras
    CardanoEras
  , CardanoShelleyEras
  , module Ouroboros.Consensus.Shelley.Eras
    -- * Block
  , CardanoBlock
    -- Note: by exporting the pattern synonyms as part of the matching data
    -- type (instead of as separate patterns), we get better exhaustiveness
    -- checks from GHC. But GHC expects a data type, not a type family, that's
    -- why we sometimes mention the data type of the instance in these exports
    -- instead of the abstract type family.
  , HardForkBlock (BlockAllegra, BlockAlonzo, BlockByron, BlockMary, BlockShelley, BlockBabbage)
    -- * Headers
  , CardanoHeader
  , Header (HeaderAllegra, HeaderAlonzo, HeaderByron, HeaderMary, HeaderShelley, HeaderBabbage)
    -- * Generalised transactions
  , CardanoApplyTxErr
  , CardanoGenTx
  , CardanoGenTxId
  , GenTx (GenTxAllegra, GenTxAlonzo, GenTxByron, GenTxMary, GenTxShelley, GenTxBabbage)
  , HardForkApplyTxErr (ApplyTxErrAllegra, ApplyTxErrAlonzo, ApplyTxErrByron, ApplyTxErrMary, ApplyTxErrShelley, ApplyTxErrWrongEra, ApplyTxErrBabbage)
  , TxId (GenTxIdAllegra, GenTxIdAlonzo, GenTxIdByron, GenTxIdMary, GenTxIdShelley, GenTxIdBabbage)
    -- * LedgerError
  , CardanoLedgerError
  , HardForkLedgerError (LedgerErrorAllegra, LedgerErrorAlonzo, LedgerErrorByron, LedgerErrorMary, LedgerErrorShelley, LedgerErrorWrongEra, LedgerErrorBabbage)
    -- * OtherEnvelopeError
  , CardanoOtherHeaderEnvelopeError
  , HardForkEnvelopeErr (OtherHeaderEnvelopeErrorAllegra, OtherHeaderEnvelopeErrorBabbage, OtherHeaderEnvelopeErrorAlonzo, OtherHeaderEnvelopeErrorByron, OtherHeaderEnvelopeErrorMary, OtherHeaderEnvelopeErrorShelley, OtherHeaderEnvelopeErrorWrongEra)
    -- * TipInfo
  , CardanoTipInfo
  , OneEraTipInfo (TipInfoAllegra, TipInfoAlonzo, TipInfoByron, TipInfoBabbage, TipInfoMary, TipInfoShelley)
    -- * Query
  , BlockQuery (QueryAnytimeAllegra, QueryAnytimeAlonzo, QueryAnytimeBabbage, QueryAnytimeByron, QueryAnytimeMary, QueryAnytimeShelley, QueryHardFork, QueryIfCurrentAllegra, QueryIfCurrentAlonzo, QueryIfCurrentBabbage, QueryIfCurrentByron, QueryIfCurrentMary, QueryIfCurrentShelley)
  , CardanoQuery
  , CardanoQueryResult
  , Either (QueryResultSuccess, QueryResultEraMismatch)
    -- * CodecConfig
  , CardanoCodecConfig
  , CodecConfig (CardanoCodecConfig)
    -- * BlockConfig
  , BlockConfig (CardanoBlockConfig)
  , CardanoBlockConfig
    -- * StorageConfig
  , CardanoStorageConfig
  , StorageConfig (CardanoStorageConfig)
    -- * ConsensusConfig
  , CardanoConsensusConfig
  , ConsensusConfig (CardanoConsensusConfig)
    -- * LedgerConfig
  , CardanoLedgerConfig
  , HardForkLedgerConfig (CardanoLedgerConfig)
    -- * LedgerState
  , CardanoLedgerState
  , LedgerState (LedgerStateAllegra, LedgerStateAlonzo, LedgerStateBabbage, LedgerStateByron, LedgerStateMary, LedgerStateShelley)
    -- * ChainDepState
  , CardanoChainDepState
  , HardForkState (ChainDepStateAllegra, ChainDepStateAlonzo, ChainDepStateBabbage, ChainDepStateByron, ChainDepStateMary, ChainDepStateShelley)
    -- * EraMismatch
  , EraMismatch (..)
  , StandardCryptoVersions
  , CryptoVersions
  , CryptoV1
  , CryptoV2
  , ShelleyBlockTPraosShelley
  , ShelleyBlockTPraosAllegra
  , ShelleyBlockTPraosMary
  , ShelleyBlockTPraosAlonzo
  , ShelleyBlockPraosBabbage
  , CardanoErasPARAMETRIC
  , CardanoShelleyErasPARAMETRIC
  ) where

import           Data.SOP.Strict

import           Cardano.Ledger.Crypto (StandardCrypto, StandardCryptoV2)

import           Ouroboros.Consensus.Block (BlockProtocol)
import           Ouroboros.Consensus.HeaderValidation (OtherHeaderEnvelopeError,
                     TipInfo)
import           Ouroboros.Consensus.Ledger.Abstract (LedgerError)
import           Ouroboros.Consensus.Ledger.SupportsMempool (ApplyTxErr,
                     GenTxId)
import           Ouroboros.Consensus.Protocol.Abstract (ChainDepState)
import           Ouroboros.Consensus.TypeFamilyWrappers

import           Ouroboros.Consensus.HardFork.Combinator
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras
import qualified Ouroboros.Consensus.HardFork.Combinator.State as State

import           Ouroboros.Consensus.Byron.Ledger.Block (ByronBlock)

import           Ouroboros.Consensus.Protocol.Praos (Praos)
import           Ouroboros.Consensus.Protocol.TPraos (TPraos)
import           Ouroboros.Consensus.Shelley.Eras
import           Ouroboros.Consensus.Shelley.Ledger (ShelleyBlock)

import           Data.Kind (Type)


data StandardCryptoVersions

class CryptoVersions c where

  type CryptoV1 c = (c1 :: Type) | c1 -> c
  type CryptoV2 c = (c2 :: Type) | c2 -> c

instance CryptoVersions StandardCryptoVersions where
  type CryptoV1 StandardCryptoVersions = StandardCrypto
  type CryptoV2 StandardCryptoVersions = StandardCryptoV2

type ShelleyBlockProtoEra p e c = ShelleyBlock (p c) (e c)

-- Nomenclature <BlockType><Protocol><Era>
type ShelleyBlockTPraosShelley c = ShelleyBlockProtoEra TPraos ShelleyEra (CryptoV1 c)
type ShelleyBlockTPraosAllegra c = ShelleyBlockProtoEra TPraos AllegraEra (CryptoV1 c)
type ShelleyBlockTPraosMary    c = ShelleyBlockProtoEra TPraos MaryEra    (CryptoV1 c)
type ShelleyBlockTPraosAlonzo  c = ShelleyBlockProtoEra TPraos AlonzoEra  (CryptoV1 c)
type ShelleyBlockPraosBabbage  c = ShelleyBlockProtoEra Praos  BabbageEra (CryptoV2 c)

{-------------------------------------------------------------------------------
  The eras of the Cardano blockchain
-------------------------------------------------------------------------------}
type CardanoEras c = CardanoErasPARAMETRIC (CryptoV1 c) (CryptoV2 c)
type CardanoShelleyEras c = CardanoShelleyErasPARAMETRIC (CryptoV1 c) (CryptoV2 c)

type CardanoErasPARAMETRIC c1 c2 = ByronBlock ': CardanoShelleyErasPARAMETRIC c1 c2

type CardanoShelleyErasPARAMETRIC c1 c2 =
  '[ ShelleyBlockC TPraos ShelleyEra c1
   , ShelleyBlockC TPraos AllegraEra c1
   , ShelleyBlockC TPraos MaryEra    c1
   , ShelleyBlockC TPraos AlonzoEra  c1
   , ShelleyBlockC Praos  BabbageEra c2
   ]

type ShelleyBlockC proto era c = ShelleyBlock (proto c) (era c)
-- | The eras in the Cardano blockchain.
--
-- We parameterise over the crypto used in the post-Byron eras: @c@.
--
-- TODO: parameterise ByronBlock over crypto too

-- type CardanoEras c = ByronBlock ': CardanoShelleyEras c

-- type CardanoShelleyEras c =
--   '[ ShelleyBlockTPraosShelley c
--    , ShelleyBlockTPraosAllegra c
--    , ShelleyBlockTPraosMary c
--    , ShelleyBlockTPraosAlonzo c
--    , ShelleyBlockPraosBabbage c
--    ]

{-------------------------------------------------------------------------------
  INTERNAL A tag function for each era
-------------------------------------------------------------------------------}

-- Here we use layout and adjacency to make it obvious that we haven't
-- miscounted.

pattern TagByron   :: f ByronBlock                                -> NS f (CardanoEras c)
pattern TagShelley :: f (ShelleyBlockTPraosShelley c)  -> NS f (CardanoEras c)
pattern TagAllegra :: f (ShelleyBlockTPraosAllegra c)  -> NS f (CardanoEras c)
pattern TagMary    :: f (ShelleyBlockTPraosMary c)  -> NS f (CardanoEras c)
pattern TagAlonzo  :: f (ShelleyBlockTPraosAlonzo c)  -> NS f (CardanoEras c)
pattern TagBabbage :: f (ShelleyBlockPraosBabbage c) -> NS f (CardanoEras c)

pattern TagByron   x =                Z x
pattern TagShelley x =             S (Z x)
pattern TagAllegra x =          S (S (Z x))
pattern TagMary    x =       S (S (S (Z x)))
pattern TagAlonzo  x =    S (S (S (S (Z x))))
pattern TagBabbage x = S (S (S (S (S (Z x)))))

{-------------------------------------------------------------------------------
  INTERNAL A telescope function for each era

-------------------------------------------------------------------------------}

pattern TeleByron   ::
     f ByronBlock
  -> Telescope g f (CardanoEras c)

pattern TeleShelley ::
     g ByronBlock
  -> f (ShelleyBlockTPraosShelley c)
  -> Telescope g f (CardanoEras c)

pattern TeleAllegra ::
     g ByronBlock
  -> g (ShelleyBlockTPraosShelley c)
  -> f (ShelleyBlockTPraosAllegra c)
  -> Telescope g f (CardanoEras c)

pattern TeleMary    ::
     g ByronBlock
  -> g (ShelleyBlockTPraosShelley c)
  -> g (ShelleyBlockTPraosAllegra c)
  -> f (ShelleyBlockTPraosMary c)
  -> Telescope g f (CardanoEras c)

pattern TeleAlonzo  ::
     g ByronBlock
  -> g (ShelleyBlockTPraosShelley c)
  -> g (ShelleyBlockTPraosAllegra c)
  -> g (ShelleyBlockTPraosMary c)
  -> f (ShelleyBlockTPraosAlonzo c)
  -> Telescope g f (CardanoEras c)

pattern TeleBabbage  ::
     g ByronBlock
  -> g (ShelleyBlockTPraosShelley c)
  -> g (ShelleyBlockTPraosAllegra c)
  -> g (ShelleyBlockTPraosMary c)
  -> g (ShelleyBlockTPraosAlonzo c)
  -> f (ShelleyBlockPraosBabbage c)
  -> Telescope g f (CardanoEras c)
-- Here we use layout and adjacency to make it obvious that we haven't
-- miscounted.

pattern TeleByron                                     x =                                                       TZ x
pattern TeleShelley byron                             x = TS byron                                             (TZ x)
pattern TeleAllegra byron shelley                     x = TS byron (TS shelley                                 (TZ x))
pattern TeleMary    byron shelley allegra             x = TS byron (TS shelley (TS allegra                     (TZ x)))
pattern TeleAlonzo  byron shelley allegra mary        x = TS byron (TS shelley (TS allegra (TS mary            (TZ x))))
pattern TeleBabbage byron shelley allegra mary alonzo x = TS byron (TS shelley (TS allegra (TS mary (TS alonzo (TZ x)))))

{-------------------------------------------------------------------------------
  The block type of the Cardano block chain
-------------------------------------------------------------------------------}

-- | /The/ Cardano block.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'BlockByron' and 'BlockShelley'.
--
-- > f :: CardanoBlock c -> _
-- > f (BlockByron   b) = _
-- > f (BlockShelley s) = _
-- > f (BlockAllegra a) = _
-- > f (BlockMary    m) = _
-- > f (BlockAlonzo  m) = _
--
type CardanoBlock c = HardForkBlock (CardanoEras c)

pattern BlockByron :: ByronBlock -> CardanoBlock c
pattern BlockByron b = HardForkBlock (OneEraBlock (TagByron (I b)))

pattern BlockShelley :: ShelleyBlockTPraosShelley c -> CardanoBlock c
pattern BlockShelley b = HardForkBlock (OneEraBlock (TagShelley (I b)))

pattern BlockAllegra :: ShelleyBlockTPraosAllegra c -> CardanoBlock c
pattern BlockAllegra b = HardForkBlock (OneEraBlock (TagAllegra (I b)))

pattern BlockMary :: ShelleyBlockTPraosMary c -> CardanoBlock c
pattern BlockMary b = HardForkBlock (OneEraBlock (TagMary (I b)))

pattern BlockAlonzo :: ShelleyBlockTPraosAlonzo c -> CardanoBlock c
pattern BlockAlonzo b = HardForkBlock (OneEraBlock (TagAlonzo (I b)))

pattern BlockBabbage :: ShelleyBlockPraosBabbage c -> CardanoBlock c
pattern BlockBabbage b = HardForkBlock (OneEraBlock (TagBabbage (I b)))

{-# COMPLETE
    BlockByron
  , BlockShelley
  , BlockAllegra
  , BlockMary
  , BlockAlonzo
  , BlockBabbage
  #-}


{-------------------------------------------------------------------------------
  Headers
-------------------------------------------------------------------------------}

-- | The Cardano header.
type CardanoHeader c = Header (CardanoBlock c)

pattern HeaderByron :: Header ByronBlock -> CardanoHeader c
pattern HeaderByron h = HardForkHeader (OneEraHeader (TagByron h))

pattern HeaderShelley ::
     Header (ShelleyBlockTPraosShelley c)
  -> CardanoHeader c
pattern HeaderShelley h = HardForkHeader (OneEraHeader (TagShelley h))

pattern HeaderAllegra ::
     Header (ShelleyBlockTPraosAllegra c)
  -> CardanoHeader c
pattern HeaderAllegra h = HardForkHeader (OneEraHeader (TagAllegra h))

pattern HeaderMary ::
     Header (ShelleyBlockTPraosMary c)
  -> CardanoHeader c
pattern HeaderMary h = HardForkHeader (OneEraHeader (TagMary h))

pattern HeaderAlonzo ::
     Header (ShelleyBlockTPraosAlonzo c)
  -> CardanoHeader c
pattern HeaderAlonzo h = HardForkHeader (OneEraHeader (TagAlonzo h))

pattern HeaderBabbage ::
     Header (ShelleyBlockPraosBabbage c)
  -> CardanoHeader c
pattern HeaderBabbage h = HardForkHeader (OneEraHeader (TagBabbage h))

{-# COMPLETE HeaderByron
           , HeaderShelley
           , HeaderAllegra
           , HeaderMary
           , HeaderAlonzo
           , HeaderBabbage #-}

{-------------------------------------------------------------------------------
  Generalised transactions
-------------------------------------------------------------------------------}

-- | The Cardano transaction.
type CardanoGenTx c = GenTx (CardanoBlock c)

pattern GenTxByron :: GenTx ByronBlock -> CardanoGenTx c
pattern GenTxByron tx = HardForkGenTx (OneEraGenTx (TagByron tx))

pattern GenTxShelley :: GenTx (ShelleyBlockTPraosShelley c) -> CardanoGenTx c
pattern GenTxShelley tx = HardForkGenTx (OneEraGenTx (TagShelley tx))

pattern GenTxAllegra :: GenTx (ShelleyBlockTPraosAllegra c) -> CardanoGenTx c
pattern GenTxAllegra tx = HardForkGenTx (OneEraGenTx (TagAllegra tx))

pattern GenTxMary :: GenTx (ShelleyBlockTPraosMary c) -> CardanoGenTx c
pattern GenTxMary tx = HardForkGenTx (OneEraGenTx (TagMary tx))

pattern GenTxAlonzo :: GenTx (ShelleyBlockTPraosAlonzo c) -> CardanoGenTx c
pattern GenTxAlonzo tx = HardForkGenTx (OneEraGenTx (TagAlonzo tx))

pattern GenTxBabbage :: GenTx (ShelleyBlockPraosBabbage c) -> CardanoGenTx c
pattern GenTxBabbage tx = HardForkGenTx (OneEraGenTx (TagBabbage tx))

{-# COMPLETE
    GenTxByron
  , GenTxShelley
  , GenTxAllegra
  , GenTxMary
  , GenTxAlonzo
  , GenTxBabbage
  #-}



-- | The ID of a Cardano transaction.
type CardanoGenTxId c = GenTxId (CardanoBlock c)

pattern GenTxIdByron :: GenTxId ByronBlock -> CardanoGenTxId c
pattern GenTxIdByron txid =
    HardForkGenTxId (OneEraGenTxId (TagByron (WrapGenTxId txid)))

pattern GenTxIdShelley ::
     GenTxId (ShelleyBlockTPraosShelley c)
  -> CardanoGenTxId c
pattern GenTxIdShelley txid =
    HardForkGenTxId (OneEraGenTxId (TagShelley (WrapGenTxId txid)))

pattern GenTxIdAllegra ::
     GenTxId (ShelleyBlockTPraosAllegra c)
  -> CardanoGenTxId c
pattern GenTxIdAllegra txid =
    HardForkGenTxId (OneEraGenTxId (TagAllegra (WrapGenTxId txid)))

pattern GenTxIdMary ::
     GenTxId (ShelleyBlockTPraosMary c)
  -> CardanoGenTxId c
pattern GenTxIdMary txid =
    HardForkGenTxId (OneEraGenTxId (TagMary (WrapGenTxId txid)))

pattern GenTxIdAlonzo ::
     GenTxId (ShelleyBlockTPraosAlonzo c)
  -> CardanoGenTxId c
pattern GenTxIdAlonzo txid =
    HardForkGenTxId (OneEraGenTxId (TagAlonzo (WrapGenTxId txid)))

pattern GenTxIdBabbage ::
     GenTxId (ShelleyBlockPraosBabbage c)
  -> CardanoGenTxId c
pattern GenTxIdBabbage txid =
    HardForkGenTxId (OneEraGenTxId (TagBabbage (WrapGenTxId txid)))

{-# COMPLETE GenTxIdByron
           , GenTxIdShelley
           , GenTxIdAllegra
           , GenTxIdMary
           , GenTxIdAlonzo
           , GenTxIdBabbage #-}

-- | An error resulting from applying a 'CardanoGenTx' to the ledger.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'ApplyTxByronErr', 'ApplyTxErrShelley', and
-- 'ApplyTxErrWrongEra'.
--
-- > toText :: CardanoApplyTxErr c -> Text
-- > toText (ApplyTxErrByron b) = byronApplyTxErrToText b
-- > toText (ApplyTxErrShelley s) = shelleyApplyTxErrToText s
-- > toText (ApplyTxErrAllegra a) = allegraApplyTxErrToText a
-- > toText (ApplyTxErrMary m) = maryApplyTxErrToText m
-- > toText (ApplyTxErrWrongEra eraMismatch) =
-- >   "Transaction from the " <> otherEraName eraMismatch <>
-- >   " era applied to a ledger from the " <>
-- >   ledgerEraName eraMismatch <> " era"
--
type CardanoApplyTxErr c = HardForkApplyTxErr (CardanoEras c)

pattern ApplyTxErrByron :: ApplyTxErr ByronBlock -> CardanoApplyTxErr c
pattern ApplyTxErrByron err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (TagByron (WrapApplyTxErr err)))

pattern ApplyTxErrShelley ::
     ApplyTxErr (ShelleyBlockTPraosShelley c)
  -> CardanoApplyTxErr c
pattern ApplyTxErrShelley err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (TagShelley (WrapApplyTxErr err)))

pattern ApplyTxErrAllegra ::
     ApplyTxErr (ShelleyBlockTPraosAllegra c)
  -> CardanoApplyTxErr c
pattern ApplyTxErrAllegra err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (TagAllegra (WrapApplyTxErr err)))

pattern ApplyTxErrMary ::
     ApplyTxErr (ShelleyBlockTPraosMary c)
  -> CardanoApplyTxErr c
pattern ApplyTxErrMary err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (TagMary (WrapApplyTxErr err)))

pattern ApplyTxErrAlonzo ::
     ApplyTxErr (ShelleyBlockTPraosAlonzo c)
  -> CardanoApplyTxErr c
pattern ApplyTxErrAlonzo err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (TagAlonzo (WrapApplyTxErr err)))

pattern ApplyTxErrBabbage ::
     ApplyTxErr (ShelleyBlockPraosBabbage c)
  -> CardanoApplyTxErr c
pattern ApplyTxErrBabbage err =
    HardForkApplyTxErrFromEra (OneEraApplyTxErr (TagBabbage (WrapApplyTxErr err)))

pattern ApplyTxErrWrongEra :: EraMismatch -> CardanoApplyTxErr c
pattern ApplyTxErrWrongEra eraMismatch <-
    HardForkApplyTxErrWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE ApplyTxErrByron
           , ApplyTxErrShelley
           , ApplyTxErrAllegra
           , ApplyTxErrMary
           , ApplyTxErrAlonzo
           , ApplyTxErrBabbage
           , ApplyTxErrWrongEra #-}

{-------------------------------------------------------------------------------
  LedgerError
-------------------------------------------------------------------------------}

-- | An error resulting from applying a 'CardanoBlock' to the ledger.
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'LedgerErrorByron', 'LedgerErrorShelley', and
-- 'LedgerErrorWrongEra'.
--
-- > toText :: CardanoLedgerError c -> Text
-- > toText (LedgerErrorByron b) = byronLedgerErrorToText b
-- > toText (LedgerErrorShelley s) = shelleyLedgerErrorToText s
-- > toText (LedgerErrorAllegra a) = allegraLedgerErrorToText a
-- > toText (LedgerErrorMary m) = maryLedgerErrorToText m
-- > toText (LedgerErrorWrongEra eraMismatch) =
-- >   "Block from the " <> otherEraName eraMismatch <>
-- >   " era applied to a ledger from the " <>
-- >   ledgerEraName eraMismatch <> " era"
--
type CardanoLedgerError c = HardForkLedgerError (CardanoEras c)

pattern LedgerErrorByron :: LedgerError ByronBlock -> CardanoLedgerError c
pattern LedgerErrorByron err =
    HardForkLedgerErrorFromEra (OneEraLedgerError (TagByron (WrapLedgerErr err)))

pattern LedgerErrorShelley ::
     LedgerError (ShelleyBlockTPraosShelley c)
  -> CardanoLedgerError c
pattern LedgerErrorShelley err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (TagShelley (WrapLedgerErr err)))

pattern LedgerErrorAllegra ::
     LedgerError (ShelleyBlockTPraosAllegra c)
  -> CardanoLedgerError c
pattern LedgerErrorAllegra err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (TagAllegra (WrapLedgerErr err)))

pattern LedgerErrorMary ::
     LedgerError (ShelleyBlockTPraosMary c)
  -> CardanoLedgerError c
pattern LedgerErrorMary err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (TagMary (WrapLedgerErr err)))

pattern LedgerErrorAlonzo ::
     LedgerError (ShelleyBlockTPraosAlonzo c)
  -> CardanoLedgerError c
pattern LedgerErrorAlonzo err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (TagAlonzo (WrapLedgerErr err)))

pattern LedgerErrorBabbage ::
     LedgerError (ShelleyBlockPraosBabbage c)
  -> CardanoLedgerError c
pattern LedgerErrorBabbage err =
    HardForkLedgerErrorFromEra
      (OneEraLedgerError (TagBabbage (WrapLedgerErr err)))

pattern LedgerErrorWrongEra :: EraMismatch -> CardanoLedgerError c
pattern LedgerErrorWrongEra eraMismatch <-
    HardForkLedgerErrorWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE LedgerErrorByron
           , LedgerErrorShelley
           , LedgerErrorAllegra
           , LedgerErrorMary
           , LedgerErrorAlonzo
           , LedgerErrorBabbage
           , LedgerErrorWrongEra #-}

{-------------------------------------------------------------------------------
  OtherEnvelopeError
-------------------------------------------------------------------------------}

-- | An error resulting from validating a 'CardanoHeader'.
type CardanoOtherHeaderEnvelopeError c = HardForkEnvelopeErr (CardanoEras c)

pattern OtherHeaderEnvelopeErrorByron
  :: OtherHeaderEnvelopeError ByronBlock
  -> CardanoOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorByron err =
    HardForkEnvelopeErrFromEra
      (OneEraEnvelopeErr (TagByron (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorShelley
  :: OtherHeaderEnvelopeError (ShelleyBlockTPraosShelley c)
  -> CardanoOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorShelley err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (TagShelley (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorAllegra
  :: OtherHeaderEnvelopeError (ShelleyBlockTPraosAllegra c)
  -> CardanoOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorAllegra err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (TagAllegra (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorMary
  :: OtherHeaderEnvelopeError (ShelleyBlockTPraosMary c)
  -> CardanoOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorMary err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (TagMary (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorAlonzo
  :: OtherHeaderEnvelopeError (ShelleyBlockTPraosAlonzo c)
  -> CardanoOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorAlonzo err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (TagAlonzo (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorBabbage
  :: OtherHeaderEnvelopeError (ShelleyBlockPraosBabbage c)
  -> CardanoOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorBabbage err =
    HardForkEnvelopeErrFromEra (OneEraEnvelopeErr (TagBabbage (WrapEnvelopeErr err)))

pattern OtherHeaderEnvelopeErrorWrongEra
  :: EraMismatch
  -> CardanoOtherHeaderEnvelopeError c
pattern OtherHeaderEnvelopeErrorWrongEra eraMismatch <-
    HardForkEnvelopeErrWrongEra (mkEraMismatch -> eraMismatch)

{-# COMPLETE OtherHeaderEnvelopeErrorByron
           , OtherHeaderEnvelopeErrorShelley
           , OtherHeaderEnvelopeErrorAllegra
           , OtherHeaderEnvelopeErrorMary
           , OtherHeaderEnvelopeErrorAlonzo
           , OtherHeaderEnvelopeErrorBabbage
           , OtherHeaderEnvelopeErrorWrongEra #-}

{-------------------------------------------------------------------------------
  TipInfo
-------------------------------------------------------------------------------}

-- | The 'TipInfo' of the Cardano chain.
type CardanoTipInfo c = OneEraTipInfo (CardanoEras c)

pattern TipInfoByron :: TipInfo ByronBlock -> CardanoTipInfo c
pattern TipInfoByron ti = OneEraTipInfo (TagByron (WrapTipInfo ti))

pattern TipInfoShelley ::
     TipInfo (ShelleyBlockTPraosShelley c)
  -> CardanoTipInfo c
pattern TipInfoShelley ti = OneEraTipInfo (TagShelley (WrapTipInfo ti))

pattern TipInfoAllegra ::
     TipInfo (ShelleyBlockTPraosAllegra c)
  -> CardanoTipInfo c
pattern TipInfoAllegra ti = OneEraTipInfo (TagAllegra (WrapTipInfo ti))

pattern TipInfoMary ::
     TipInfo (ShelleyBlockTPraosMary c)
  -> CardanoTipInfo c
pattern TipInfoMary ti = OneEraTipInfo (TagMary (WrapTipInfo ti))

pattern TipInfoAlonzo ::
     TipInfo (ShelleyBlockTPraosAlonzo c)
  -> CardanoTipInfo c
pattern TipInfoAlonzo ti = OneEraTipInfo (TagAlonzo (WrapTipInfo ti))

pattern TipInfoBabbage ::
     TipInfo (ShelleyBlockPraosBabbage c)
  -> CardanoTipInfo c
pattern TipInfoBabbage ti = OneEraTipInfo (TagBabbage (WrapTipInfo ti))

{-# COMPLETE TipInfoByron
           , TipInfoShelley
           , TipInfoAllegra
           , TipInfoMary
           , TipInfoAlonzo
           , TipInfoBabbage #-}

{-------------------------------------------------------------------------------
  Query
-------------------------------------------------------------------------------}

-- | The 'Query' of Cardano chain.
type CardanoQuery c = BlockQuery (CardanoBlock c)

-- | Byron-specific query that can only be answered when the ledger is in the
-- Byron era.
pattern QueryIfCurrentByron
  :: ()
  => CardanoQueryResult c result ~ a
  => BlockQuery ByronBlock result
  -> CardanoQuery c a

-- | Shelley-specific query that can only be answered when the ledger is in the
-- Shelley era.
pattern QueryIfCurrentShelley
  :: ()
  => CardanoQueryResult c result ~ a
  => BlockQuery (ShelleyBlockTPraosShelley c) result
  -> CardanoQuery c a

-- | Allegra-specific query that can only be answered when the ledger is in the
-- Allegra era.
pattern QueryIfCurrentAllegra
  :: ()
  => CardanoQueryResult c result ~ a
  => BlockQuery (ShelleyBlockTPraosAllegra c) result
  -> CardanoQuery c a

-- | Mary-specific query that can only be answered when the ledger is in the
-- Mary era.
pattern QueryIfCurrentMary
  :: ()
  => CardanoQueryResult c result ~ a
  => BlockQuery (ShelleyBlockTPraosMary c) result
  -> CardanoQuery c a

-- | Alonzo-specific query that can only be answered when the ledger is in the
-- Alonzo era.
pattern QueryIfCurrentAlonzo
  :: ()
  => CardanoQueryResult c result ~ a
  => BlockQuery (ShelleyBlockTPraosAlonzo c) result
  -> CardanoQuery c a

-- | Babbage-specific query that can only be answered when the ledger is in the
-- Babbage era.
pattern QueryIfCurrentBabbage
  :: ()
  => CardanoQueryResult c result ~ a
  => BlockQuery (ShelleyBlockPraosBabbage c) result
  -> CardanoQuery c a

-- Here we use layout and adjacency to make it obvious that we haven't
-- miscounted.

pattern QueryIfCurrentByron   q = QueryIfCurrent                     (QZ q)
pattern QueryIfCurrentShelley q = QueryIfCurrent                 (QS (QZ q))
pattern QueryIfCurrentAllegra q = QueryIfCurrent             (QS (QS (QZ q)))
pattern QueryIfCurrentMary    q = QueryIfCurrent         (QS (QS (QS (QZ q))))
pattern QueryIfCurrentAlonzo  q = QueryIfCurrent     (QS (QS (QS (QS (QZ q)))))
pattern QueryIfCurrentBabbage q = QueryIfCurrent (QS (QS (QS (QS (QS (QZ q))))))

-- | Query about the Byron era that can be answered anytime, i.e.,
-- independent from where the tip of the ledger is.
--
-- For example, to ask for the start of the Byron era (whether the tip of
-- the ledger is in the Byron, Shelley, ... era), use:
--
-- > QueryAnytimeByron EraStart
--
pattern QueryAnytimeByron
  :: QueryAnytime result
  -> CardanoQuery c result
pattern QueryAnytimeByron q = QueryAnytime q (EraIndex (TagByron (K ())))

-- | Query about the Shelley era that can be answered anytime, i.e.,
-- independent from where the tip of the ledger is.
--
-- For example, to ask for the start of the Shelley era (whether the tip of the
-- ledger is in the Byron, Shelley, ... era), use:
--
-- > QueryAnytimeShelley EraStart
--
pattern QueryAnytimeShelley
  :: QueryAnytime result
  -> CardanoQuery c result
pattern QueryAnytimeShelley q = QueryAnytime q (EraIndex (TagShelley (K ())))

-- | Query about the Allegra era that can be answered anytime, i.e.,
-- independent from where the tip of the ledger is.
--
-- For example, to ask for the start of the Allegra era (whether the tip of the
-- ledger is in the Byron, Shelley, ... era), use:
--
-- > QueryAnytimeAllegra EraStart
--
pattern QueryAnytimeAllegra
  :: QueryAnytime result
  -> CardanoQuery c result
pattern QueryAnytimeAllegra q = QueryAnytime q (EraIndex (TagAllegra (K ())))

-- | Query about the Mary era that can be answered anytime, i.e.,
-- independent from where the tip of the ledger is.
--
-- For example, to ask for the start of the Mary era (whether the tip of the
-- ledger is in the Byron, Shelley, ... era), use:
--
-- > QueryAnytimeMary EraStart
--
pattern QueryAnytimeMary
  :: QueryAnytime result
  -> CardanoQuery c result
pattern QueryAnytimeMary q = QueryAnytime q (EraIndex (TagMary (K ())))

-- | Query about the Alonzo era that can be answered anytime, i.e., independent
-- from where the tip of the ledger is.
--
-- For example, to ask for the start of the Alonzo era (whether the tip of the
-- ledger is in the Byron, Shelley, ... era), use:
--
-- > QueryAnytimeAlonzo EraStart
--
pattern QueryAnytimeAlonzo
  :: QueryAnytime result
  -> CardanoQuery c result
pattern QueryAnytimeAlonzo q = QueryAnytime q (EraIndex (TagAlonzo (K ())))

-- | Query about the Babbage era that can be answered anytime, i.e., independent
-- from where the tip of the ledger is.
--
-- For example, to ask for the start of the Babbage era (whether the tip of the
-- ledger is in the Byron, Shelley, ... era), use:
--
-- > QueryAnytimeBabbage EraStart
--
pattern QueryAnytimeBabbage
  :: QueryAnytime result
  -> CardanoQuery c result
pattern QueryAnytimeBabbage q = QueryAnytime q (EraIndex (TagBabbage (K ())))

{-# COMPLETE QueryIfCurrentByron
           , QueryIfCurrentShelley
           , QueryIfCurrentAllegra
           , QueryIfCurrentMary
           , QueryIfCurrentAlonzo
           , QueryIfCurrentBabbage
           , QueryAnytimeByron
           , QueryAnytimeShelley
           , QueryAnytimeAllegra
           , QueryAnytimeMary
           , QueryAnytimeAlonzo
           , QueryAnytimeBabbage
           , QueryHardFork #-}

-- | The result of a 'CardanoQuery'
--
-- Thanks to the pattern synonyms, you can treat this as a sum type with
-- constructors 'QueryResultSuccess' and 'QueryResultEraMismatch'.
type CardanoQueryResult c = HardForkQueryResult (CardanoEras c)

pattern QueryResultSuccess :: result -> CardanoQueryResult c result
pattern QueryResultSuccess result = Right result

-- | A query from a different era than the ledger's era was sent.
pattern QueryResultEraMismatch :: EraMismatch -> CardanoQueryResult c result
pattern QueryResultEraMismatch eraMismatch <- Left (mkEraMismatch -> eraMismatch)

{-# COMPLETE QueryResultSuccess, QueryResultEraMismatch #-}

{-------------------------------------------------------------------------------
  CodecConfig
-------------------------------------------------------------------------------}

-- | The 'CodecConfig' for 'CardanoBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of
-- the Byron, Shelley, ... 'CodecConfig's.
type CardanoCodecConfig c = CodecConfig (CardanoBlock c)

pattern CardanoCodecConfig
  :: CodecConfig ByronBlock
  -> CodecConfig (ShelleyBlockTPraosShelley c)
  -> CodecConfig (ShelleyBlockTPraosAllegra c)
  -> CodecConfig (ShelleyBlockTPraosMary c)
  -> CodecConfig (ShelleyBlockTPraosAlonzo c)
  -> CodecConfig (ShelleyBlockPraosBabbage c)
  -> CardanoCodecConfig c
pattern CardanoCodecConfig cfgByron cfgShelley cfgAllegra cfgMary cfgAlonzo cfgBabbage =
    HardForkCodecConfig {
        hardForkCodecConfigPerEra = PerEraCodecConfig
          (  cfgByron
          :* cfgShelley
          :* cfgAllegra
          :* cfgMary
          :* cfgAlonzo
          :* cfgBabbage
          :* Nil
          )
      }

{-# COMPLETE CardanoCodecConfig #-}

{-------------------------------------------------------------------------------
  BlockConfig
-------------------------------------------------------------------------------}

-- | The 'BlockConfig' for 'CardanoBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of
-- the Byron, Shelley, ... 'BlockConfig's.
type CardanoBlockConfig c = BlockConfig (CardanoBlock c)

pattern CardanoBlockConfig
  :: BlockConfig ByronBlock
  -> BlockConfig (ShelleyBlockTPraosShelley c)
  -> BlockConfig (ShelleyBlockTPraosAllegra c)
  -> BlockConfig (ShelleyBlockTPraosMary c)
  -> BlockConfig (ShelleyBlockTPraosAlonzo c)
  -> BlockConfig (ShelleyBlockPraosBabbage c)
  -> CardanoBlockConfig c
pattern CardanoBlockConfig cfgByron cfgShelley cfgAllegra cfgMary cfgAlonzo cfgBabbage =
    HardForkBlockConfig {
        hardForkBlockConfigPerEra = PerEraBlockConfig
          (  cfgByron
          :* cfgShelley
          :* cfgAllegra
          :* cfgMary
          :* cfgAlonzo
          :* cfgBabbage
          :* Nil
          )
      }

{-# COMPLETE CardanoBlockConfig #-}

{-------------------------------------------------------------------------------
  StorageConfig
-------------------------------------------------------------------------------}

-- | The 'StorageConfig' for 'CardanoBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of
-- the Byron, Shelley, ... 'StorageConfig's.
type CardanoStorageConfig c = StorageConfig (CardanoBlock c)

pattern CardanoStorageConfig
  :: StorageConfig ByronBlock
  -> StorageConfig (ShelleyBlockTPraosShelley c)
  -> StorageConfig (ShelleyBlockTPraosAllegra c)
  -> StorageConfig (ShelleyBlockTPraosMary c)
  -> StorageConfig (ShelleyBlockTPraosAlonzo c)
  -> StorageConfig (ShelleyBlockPraosBabbage c)
  -> CardanoStorageConfig c
pattern CardanoStorageConfig cfgByron cfgShelley cfgAllegra cfgMary cfgAlonzo cfgBabbage =
    HardForkStorageConfig {
        hardForkStorageConfigPerEra = PerEraStorageConfig
          (  cfgByron
          :* cfgShelley
          :* cfgAllegra
          :* cfgMary
          :* cfgAlonzo
          :* cfgBabbage
          :* Nil
          )
      }

{-# COMPLETE CardanoStorageConfig #-}

{-------------------------------------------------------------------------------
  ConsensusConfig
-------------------------------------------------------------------------------}

-- | The 'ConsensusConfig' for 'CardanoBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of the
-- Byron, Shelley, ... 'PartialConsensusConfig's.
--
-- NOTE: not 'ConsensusConfig', but 'PartialConsensusConfig'.
type CardanoConsensusConfig c =
  ConsensusConfig (HardForkProtocol (CardanoEras c))

pattern CardanoConsensusConfig
  :: PartialConsensusConfig (BlockProtocol ByronBlock)
  -> PartialConsensusConfig (BlockProtocol (ShelleyBlockTPraosShelley c))
  -> PartialConsensusConfig (BlockProtocol (ShelleyBlockTPraosAllegra c))
  -> PartialConsensusConfig (BlockProtocol (ShelleyBlockTPraosMary c))
  -> PartialConsensusConfig (BlockProtocol (ShelleyBlockTPraosAlonzo c))
  -> PartialConsensusConfig (BlockProtocol (ShelleyBlockPraosBabbage c))
  -> CardanoConsensusConfig c
pattern CardanoConsensusConfig cfgByron cfgShelley cfgAllegra cfgMary cfgAlonzo cfgBabbage <-
    HardForkConsensusConfig {
        hardForkConsensusConfigPerEra = PerEraConsensusConfig
          (  WrapPartialConsensusConfig cfgByron
          :* WrapPartialConsensusConfig cfgShelley
          :* WrapPartialConsensusConfig cfgAllegra
          :* WrapPartialConsensusConfig cfgMary
          :* WrapPartialConsensusConfig cfgAlonzo
          :* WrapPartialConsensusConfig cfgBabbage
          :* Nil
          )
      }

{-# COMPLETE CardanoConsensusConfig #-}

{-------------------------------------------------------------------------------
  LedgerConfig
-------------------------------------------------------------------------------}

-- | The 'LedgerConfig' for 'CardanoBlock'.
--
-- Thanks to the pattern synonyms, you can treat this as the product of the
-- Byron, Shelley, ... 'PartialLedgerConfig's.
--
-- NOTE: not 'LedgerConfig', but 'PartialLedgerConfig'.
type CardanoLedgerConfig c = HardForkLedgerConfig (CardanoEras c)

pattern CardanoLedgerConfig
  :: PartialLedgerConfig ByronBlock
  -> PartialLedgerConfig (ShelleyBlockTPraosShelley c)
  -> PartialLedgerConfig (ShelleyBlockTPraosAllegra c)
  -> PartialLedgerConfig (ShelleyBlockTPraosMary c)
  -> PartialLedgerConfig (ShelleyBlockTPraosAlonzo c)
  -> PartialLedgerConfig (ShelleyBlockPraosBabbage c)
  -> CardanoLedgerConfig c
pattern CardanoLedgerConfig cfgByron cfgShelley cfgAllegra cfgMary cfgAlonzo cfgBabbage <-
    HardForkLedgerConfig {
        hardForkLedgerConfigPerEra = PerEraLedgerConfig
          (  WrapPartialLedgerConfig cfgByron
          :* WrapPartialLedgerConfig cfgShelley
          :* WrapPartialLedgerConfig cfgAllegra
          :* WrapPartialLedgerConfig cfgMary
          :* WrapPartialLedgerConfig cfgAlonzo
          :* WrapPartialLedgerConfig cfgBabbage
          :* Nil
          )
      }

{-# COMPLETE CardanoLedgerConfig #-}

{-------------------------------------------------------------------------------
  LedgerState
-------------------------------------------------------------------------------}

-- | The 'LedgerState' for 'CardanoBlock'.
--
-- NOTE: the 'CardanoLedgerState' contains more than just the current era's
-- 'LedgerState'. We don't give access to those internal details through the
-- pattern synonyms. This is also the reason the pattern synonyms are not
-- bidirectional.
type CardanoLedgerState c = LedgerState (CardanoBlock c)

pattern LedgerStateByron
  :: LedgerState ByronBlock
  -> CardanoLedgerState c
pattern LedgerStateByron st <-
    HardForkLedgerState
      (State.HardForkState
        (TeleByron (State.Current { currentState = st })))

pattern LedgerStateShelley
  :: LedgerState (ShelleyBlockTPraosShelley c)
  -> CardanoLedgerState c
pattern LedgerStateShelley st <-
    HardForkLedgerState
      (State.HardForkState
        (TeleShelley _ (State.Current { currentState = st })))

pattern LedgerStateAllegra
  :: LedgerState (ShelleyBlockTPraosAllegra c)
  -> CardanoLedgerState c
pattern LedgerStateAllegra st <-
    HardForkLedgerState
      (State.HardForkState
        (TeleAllegra _ _  (State.Current { currentState = st })))

pattern LedgerStateMary
  :: LedgerState (ShelleyBlockTPraosMary c)
  -> CardanoLedgerState c
pattern LedgerStateMary st <-
    HardForkLedgerState
      (State.HardForkState
        (TeleMary _ _ _ (State.Current { currentState = st })))

pattern LedgerStateAlonzo
  :: LedgerState (ShelleyBlockTPraosAlonzo c)
  -> CardanoLedgerState c
pattern LedgerStateAlonzo st <-
    HardForkLedgerState
      (State.HardForkState
        (TeleAlonzo _ _ _ _ (State.Current { currentState = st })))

pattern LedgerStateBabbage
  :: LedgerState (ShelleyBlockPraosBabbage c)
  -> CardanoLedgerState c
pattern LedgerStateBabbage st <-
    HardForkLedgerState
      (State.HardForkState
        (TeleBabbage _ _ _ _ _ (State.Current { currentState = st })))

{-# COMPLETE LedgerStateByron
           , LedgerStateShelley
           , LedgerStateAllegra
           , LedgerStateMary
           , LedgerStateAlonzo
           , LedgerStateBabbage #-}

{-------------------------------------------------------------------------------
  ChainDepState
-------------------------------------------------------------------------------}

-- | The 'ChainDepState' for 'CardanoBlock'.
--
-- NOTE: the 'CardanoChainDepState' contains more than just the current era's
-- 'ChainDepState'. We don't give access to those internal details through the
-- pattern synonyms. This is also the reason the pattern synonyms are not
-- bidirectional.
type CardanoChainDepState c = HardForkChainDepState (CardanoEras c)

pattern ChainDepStateByron
  :: ChainDepState (BlockProtocol ByronBlock)
  -> CardanoChainDepState c
pattern ChainDepStateByron st <-
    State.HardForkState
      (TeleByron (State.Current { currentState = WrapChainDepState st }))

pattern ChainDepStateShelley
  :: ChainDepState (BlockProtocol (ShelleyBlockTPraosShelley c))
  -> CardanoChainDepState c
pattern ChainDepStateShelley st <-
    State.HardForkState
      (TeleShelley _ (State.Current { currentState = WrapChainDepState st }))

pattern ChainDepStateAllegra
  :: ChainDepState (BlockProtocol (ShelleyBlockTPraosAllegra c))
  -> CardanoChainDepState c
pattern ChainDepStateAllegra st <-
    State.HardForkState
      (TeleAllegra _ _ (State.Current { currentState = WrapChainDepState st }))

pattern ChainDepStateMary
  :: ChainDepState (BlockProtocol (ShelleyBlockTPraosMary c))
  -> CardanoChainDepState c
pattern ChainDepStateMary st <-
    State.HardForkState
      (TeleMary _ _ _ (State.Current { currentState = WrapChainDepState st }))

pattern ChainDepStateAlonzo
  :: ChainDepState (BlockProtocol (ShelleyBlockTPraosAlonzo c))
  -> CardanoChainDepState c
pattern ChainDepStateAlonzo st <-
    State.HardForkState
      (TeleAlonzo _ _ _ _ (State.Current { currentState = WrapChainDepState st }))

pattern ChainDepStateBabbage
  :: ChainDepState (BlockProtocol (ShelleyBlockPraosBabbage c))
  -> CardanoChainDepState c
pattern ChainDepStateBabbage st <-
    State.HardForkState
      (TeleBabbage _ _ _ _ _ (State.Current { currentState = WrapChainDepState st }))

{-# COMPLETE ChainDepStateByron
           , ChainDepStateShelley
           , ChainDepStateAllegra
           , ChainDepStateMary
           , ChainDepStateAlonzo
           , ChainDepStateBabbage #-}
