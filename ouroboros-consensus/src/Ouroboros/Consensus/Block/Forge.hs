{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Ouroboros.Consensus.Block.Forge (
    -- * ForgeState
    ForgeState(..)
  , castForgeState
    -- * CanForge
  , CanForge(..)
    -- * MaintainForgeState
  , MaintainForgeState(..)
  , defaultMaintainForgeState
  , defaultMaintainNoExtraForgeState
  , hoistMaintainForgeState
  , castMaintainForgeState
  ) where

import           Data.Proxy (Proxy (..))
import           GHC.Generics (Generic)

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Consensus.Block.Abstract
import           Ouroboros.Consensus.Config
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.SupportsMempool
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util (Trivial (..), (..:))
import           Ouroboros.Consensus.Util.IOLike (IOLike)

{-------------------------------------------------------------------------------
  ForgeState
-------------------------------------------------------------------------------}

data ForgeState blk = ForgeState {
      chainIndepState :: !(ChainIndepState (BlockProtocol blk))
    , extraForgeState :: !(ExtraForgeState blk)
    }
  deriving (Generic)

deriving instance ( NoUnexpectedThunks (ExtraForgeState blk)
                  , HasChainIndepState (BlockProtocol blk)
                  ) => NoUnexpectedThunks (ForgeState blk)
deriving instance ( Show (ExtraForgeState blk)
                  , HasChainIndepState (BlockProtocol blk)
                  ) => Show (ForgeState blk)

castForgeState ::
     ( ChainIndepState (BlockProtocol blk) ~ ChainIndepState (BlockProtocol blk')
     , ExtraForgeState blk ~ ExtraForgeState blk'
     )
  => ForgeState blk -> ForgeState blk'
castForgeState ForgeState {..} = ForgeState {..}

{-------------------------------------------------------------------------------
  CanForge
-------------------------------------------------------------------------------}

class ( NoUnexpectedThunks (ExtraForgeState blk)
      , ConsensusProtocol (BlockProtocol blk)
      , Show (ExtraForgeState blk)
      ) => CanForge blk where

  -- | Extra block-specific state required to forge blocks in addition to the
  -- protocol's chain independent state.
  type ExtraForgeState blk :: *
  type ExtraForgeState blk = ()

  -- | Forge a new block
  --
  -- The forge state that is passed to 'forgeBlock' will already have been
  -- updated.
  forgeBlock
    :: TopLevelConfig blk
    -> ForgeState blk
    -> BlockNo                -- ^ Current block number
    -> SlotNo                 -- ^ Current slot number
    -> TickedLedgerState blk  -- ^ Current ledger
    -> [GenTx blk]            -- ^ Txs to add in the block
    -> IsLeader (BlockProtocol blk)
    -> blk

{-------------------------------------------------------------------------------
  Maintaining the 'ForgeState'
-------------------------------------------------------------------------------}

data MaintainForgeState (m :: * -> *) blk = MaintainForgeState {
      -- | Initial forge state
      initForgeState   :: ForgeState blk

      -- | Update the forge state
      --
      -- This function is the reason that 'MaintainForgeState' is a record:
      -- this function may have all kinds of things in its closure; for example,
      -- we might need access to some external hardware crypto hardware
      -- device.
    , updateForgeState :: ChainIndepStateConfig (BlockProtocol blk)
                       -> SlotNo  -- Current wallclock slot
                       -> ForgeState blk
                       -> m (ForgeState blk)
    }

defaultMaintainForgeState ::
     forall m blk.
     ( Trivial (ChainIndepState (BlockProtocol blk))
     , Trivial (ExtraForgeState blk)
     , Monad m
     )
  => MaintainForgeState m blk
defaultMaintainForgeState = MaintainForgeState {
      initForgeState   = ForgeState {
          chainIndepState = trivial (Proxy @(ChainIndepState (BlockProtocol blk)))
        , extraForgeState = trivial (Proxy @(ExtraForgeState blk))
        }
    , updateForgeState = \_ _ -> return
    }

-- | Default implementation of 'MaintainForgeState' in case there is no
-- 'ExtraForgeState'. 'updateForgeState' will just call
-- 'updateChainIndepState'.
defaultMaintainNoExtraForgeState ::
     forall m blk.
     ( Trivial (ExtraForgeState blk)
     , ConsensusProtocol (BlockProtocol blk)
     , IOLike m
     )
  => ChainIndepState (BlockProtocol blk)
  -> MaintainForgeState m blk
defaultMaintainNoExtraForgeState initChainIndepState = MaintainForgeState {
      initForgeState   = ForgeState {
          chainIndepState = initChainIndepState
        , extraForgeState = trivial (Proxy @(ExtraForgeState blk))
        }
    , updateForgeState = \cfg slot (ForgeState chainIndepState extraForgeState) ->
        (`ForgeState` extraForgeState) <$>
          updateChainIndepState
            (Proxy @(BlockProtocol blk))
            cfg
            slot
            chainIndepState
    }

hoistMaintainForgeState ::
     (forall x. m x -> n x)
  -> MaintainForgeState m blk
  -> MaintainForgeState n blk
hoistMaintainForgeState hoist MaintainForgeState {..} = MaintainForgeState {
      initForgeState   = initForgeState
    , updateForgeState = hoist ..: updateForgeState
    }

castMaintainForgeState ::
     ( ChainIndepStateConfig (BlockProtocol blk) ~ ChainIndepStateConfig (BlockProtocol blk')
     , ChainIndepState       (BlockProtocol blk) ~ ChainIndepState       (BlockProtocol blk')
     , ExtraForgeState blk ~ ExtraForgeState blk'
     , Functor m
     )
  => MaintainForgeState m blk -> MaintainForgeState m blk'
castMaintainForgeState maintainForgeState = MaintainForgeState {
      initForgeState   = castForgeState $ initForgeState maintainForgeState
    , updateForgeState = \cfg slot ->
          fmap castForgeState
        . updateForgeState maintainForgeState cfg slot
        . castForgeState
    }
