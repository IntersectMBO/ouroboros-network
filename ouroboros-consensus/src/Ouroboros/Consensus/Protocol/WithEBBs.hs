{-# LANGUAGE BangPatterns            #-}
{-# LANGUAGE DeriveAnyClass          #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE StandaloneDeriving      #-}
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Ouroboros.Consensus.Protocol.WithEBBs
  ( WithEBBs
  , HeaderSupportsWithEBB(..)
  , NodeConfig(..)
  -- * ChainStateWithEBBs
  , ChainStateWithEBBs   -- opaque
  , mkChainStateWithEBBs
  , decodeChainStateWithEBBs
  , encodeChainStateWithEBBs
  , initChainStateWithEBBs
  )
where

import           GHC.Generics (Generic)

import qualified Codec.CBOR.Decoding as CBOR
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.Serialise as CBOR

import           Cardano.Prelude (NoUnexpectedThunks)

import           Ouroboros.Network.Block (HasHeader (..), SlotNo)
import           Ouroboros.Network.Point (WithOrigin (..))

import           Ouroboros.Consensus.Protocol.Abstract

class ( HasHeader hoe
      , SupportedHeader p (HeaderOfBlock hoe)
      ) => HeaderSupportsWithEBB p hoe where

  type HeaderOfBlock hoe :: *
  type HeaderOfEBB hoe :: *

  eitherHeaderOrEbb :: proxy p -> hoe -> Either (HeaderOfEBB hoe) (HeaderOfBlock hoe)

-- | Extension of protocol @p@ to support addition 'epoch boundary blocks'; that
-- is, blocks which do not contribute to the general running of the protocol,
-- but do add to the hash chain.
--
-- This is entirely a legacy concern, and should go away after the Byron era.
data WithEBBs p

instance OuroborosTag p => OuroborosTag (WithEBBs p) where
  --
  -- Most types remain the same
  --

  type ChainState      (WithEBBs p) = ChainStateWithEBBs p
  type NodeState       (WithEBBs p) = NodeState      p
  type LedgerView      (WithEBBs p) = LedgerView     p
  type ValidationErr   (WithEBBs p) = ValidationErr  p
  type IsLeader        (WithEBBs p) = IsLeader       p

  type SupportedHeader (WithEBBs p) = HeaderSupportsWithEBB p

  newtype NodeConfig   (WithEBBs p) = WithEBBNodeConfig {unWithEBBNodeConfig :: NodeConfig p}
    deriving (Generic)

  preferCandidate       (WithEBBNodeConfig cfg) = preferCandidate       cfg
  compareCandidates     (WithEBBNodeConfig cfg) = compareCandidates     cfg
  protocolSecurityParam (WithEBBNodeConfig cfg) = protocolSecurityParam cfg
  protocolNetworkMagic  (WithEBBNodeConfig cfg) = protocolNetworkMagic  cfg

  applyChainState (WithEBBNodeConfig cfg) lv b cs = case
    eitherHeaderOrEbb cfg b of
      Right hdr ->
          wrap <$> applyChainState @p cfg lv hdr (underlyingChainState cs)
        where
          wrap = mkChainStateWithEBBs $ Just $
                   case firstNonEBBSlot cs of
                     Just s  -> s
                     Nothing -> blockSlot b

      -- Currently the hash chain is maintained in the ledger state, so we just
      -- ignore the EBB for protocol concerns.
      Left _ -> return cs

  checkIsLeader (WithEBBNodeConfig cfg) slot lv cs =
    checkIsLeader cfg slot lv (underlyingChainState cs)

  rewindChainState (WithEBBNodeConfig cfg) cs mSlot =
      wrap <$> rewindChainState cfg (underlyingChainState cs) mSlot'
    where
      effectivelyRewindingToOrigin = case mSlot of
        At slot -> case firstNonEBBSlot cs of
          Just s  -> slot < s
          Nothing -> True
        Origin  -> True

      (mSlot', firstNonEBBSlot')
         | effectivelyRewindingToOrigin = (Origin, Nothing)
         | otherwise                    = (mSlot, firstNonEBBSlot cs)

      wrap = mkChainStateWithEBBs firstNonEBBSlot'

instance OuroborosTag p => NoUnexpectedThunks (NodeConfig (WithEBBs p))
 -- use generic instance

-- | A wrapper around @'ChainState' p@ with extra bookeeping data
--
-- The bookkeeping data is used to avoid the #925 corner case. The @WithEBBs@
-- 'applyChainState' crucially does not pass EBBs to the underlying protocol.
-- Because the underlying 'applyChainState' never receives EBBs, the underlying
-- 'rewindChainstate' may in general fail when rewinding to an EBB's slot. In
-- #925 specifically, the underlying @PBFT@ was failing to rewind to a slot 0
-- EBB with genesis predecessor because @PBFT@'s @rewindChainState@ refuses to
-- return an empty chain state unless 'Origin' was specified directly.
--
-- Constructor marked as unsafe because we have to make sure to force that
-- slot number.
--
-- TODO: I thought that we decided we could get rid of this bookkeeping
-- altogether..?
data ChainStateWithEBBs p = UnsafeChainStateWithEBBs
  { firstNonEBBSlot      :: !(Maybe SlotNo)
    -- ^ earliest slot incorporated into 'underlyingChainState' that is not an
    -- EBB
  , underlyingChainState :: !(ChainState p)
  }
  deriving (Generic)

mkChainStateWithEBBs :: Maybe SlotNo -> ChainState p -> ChainStateWithEBBs p
mkChainStateWithEBBs mSlot underlying =
    case mSlot of
      Nothing    -> UnsafeChainStateWithEBBs Nothing     underlying
      Just !slot -> UnsafeChainStateWithEBBs (Just slot) underlying

deriving instance OuroborosTag p => NoUnexpectedThunks (ChainStateWithEBBs p)
deriving instance OuroborosTag p => Show (ChainStateWithEBBs p)
deriving instance Eq (ChainState p) => Eq (ChainStateWithEBBs p)

-- | Create a 'ChainStateWithEBBs' from a 'ChainState' that does not include
-- any non-EBB blocks.
--
initChainStateWithEBBs :: ChainState p -> ChainStateWithEBBs p
initChainStateWithEBBs = mkChainStateWithEBBs Nothing

encodeChainStateWithEBBs ::
    (ChainState p -> CBOR.Encoding)
  -> ChainStateWithEBBs p
  -> CBOR.Encoding
encodeChainStateWithEBBs f (UnsafeChainStateWithEBBs x0 x1) =
  CBOR.encodeListLen 2
  <> CBOR.encode x0
  <> f x1

decodeChainStateWithEBBs ::
     CBOR.Decoder s (ChainState p)
  -> CBOR.Decoder s (ChainStateWithEBBs p)
decodeChainStateWithEBBs f = do
  CBOR.decodeListLenOf 2
  mkChainStateWithEBBs <$> CBOR.decode <*> f
