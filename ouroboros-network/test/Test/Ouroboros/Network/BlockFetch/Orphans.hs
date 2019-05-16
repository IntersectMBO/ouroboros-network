{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Test.Ouroboros.Network.BlockFetch.Orphans () where

import           GHC.Generics (Generic)
import           Data.Aeson
import           Data.FingerTree

import           Ouroboros.Network.Block
import           Ouroboros.Network.BlockFetch
import           Ouroboros.Network.BlockFetch.ClientState
import           Ouroboros.Network.BlockFetch.Decision (FetchDecline (..), FetchMode (..))
import           Ouroboros.Network.BlockFetch.DeltaQ
import           Ouroboros.Network.ChainFragment
import           Ouroboros.Network.Protocol.BlockFetch.Type
import           Ouroboros.Network.Testing.ConcreteBlock
import           Network.TypedProtocol.Codec
import           Network.TypedProtocol.Driver

deriving instance ToJSON ConcreteHeaderHash
deriving instance ToJSON (ChainHash BlockHeader)

deriving instance ToJSON SlotNo

deriving instance Generic (Point BlockHeader)
deriving instance ToJSON (Point BlockHeader)


deriving instance Generic FetchMode
deriving instance ToJSON FetchMode
deriving instance Generic FetchDecline
deriving instance ToJSON FetchDecline


deriving instance Generic (TraceLabelPeer Int (FetchDecision [Point BlockHeader]))
deriving instance ToJSON (TraceLabelPeer Int (FetchDecision [Point BlockHeader]))



instance ToJSON (FingerTree BlockMeasure BlockHeader) where
    toJSON _ = Null -- maybe as a list
deriving instance Generic (ChainFragment BlockHeader)
deriving instance ToJSON (ChainFragment BlockHeader)
deriving instance Generic (FetchRequest BlockHeader)
deriving instance ToJSON (FetchRequest BlockHeader)

deriving instance Generic (PeerFetchInFlight BlockHeader)
deriving instance ToJSON (PeerFetchInFlight BlockHeader)

deriving instance Generic (PeerFetchStatus BlockHeader)
deriving instance ToJSON (PeerFetchStatus BlockHeader)

deriving instance Generic PeerFetchInFlightLimits
deriving instance ToJSON PeerFetchInFlightLimits

deriving instance Generic (TraceFetchClientState BlockHeader)
deriving instance ToJSON (TraceFetchClientState BlockHeader)

deriving instance Generic (TraceLabelPeer Int (TraceFetchClientState BlockHeader))
deriving instance ToJSON (TraceLabelPeer Int (TraceFetchClientState BlockHeader))



instance ToJSON (Message (BlockFetch BlockHeader Block) st st') where
    toJSON (MsgRequestRange _range) = String "MsgRequestRange" -- ++ toJSON range
    toJSON MsgStartBatch           = String "MsgStartBatch"
    toJSON (MsgBlock _block)        = String "MsgBlock " -- ++ toJSON block
    toJSON MsgNoBlocks             = String "MsgNoBlocks"
    toJSON MsgBatchDone            = String "MsgBatchDone"
    toJSON MsgClientDone           = String "MsgClientDone"

instance (forall st st'. ToJSON (Message ps st st')) => ToJSON (AnyMessage ps) where
    toJSON (AnyMessage msg) = toJSON msg

deriving instance Generic (TraceSendRecv (BlockFetch BlockHeader Block))
deriving instance ToJSON (TraceSendRecv (BlockFetch BlockHeader Block))

deriving instance Generic (TraceLabelPeer Int (TraceSendRecv (BlockFetch BlockHeader Block)))
deriving instance ToJSON (TraceLabelPeer Int (TraceSendRecv (BlockFetch BlockHeader Block)))
