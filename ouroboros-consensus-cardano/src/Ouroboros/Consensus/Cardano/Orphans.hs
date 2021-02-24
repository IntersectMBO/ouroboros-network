{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ouroboros.Consensus.Cardano.Orphans () where

import           Cardano.Binary
import           Cardano.Chain.Common (Address, BlockCount, CompactAddress,
                     KeyHash, Lovelace)
import           Cardano.Chain.Delegation (Certificate)
import           Cardano.Chain.Genesis
import           Cardano.Chain.UTxO
import           Cardano.Chain.Update (ProtocolParameters)
import           Cardano.Crypto.ProtocolMagic
import           Cardano.Crypto.Signing.Redeem
import           Data.Map (Map)
import           Data.Set (Set)
import           Data.Word (Word64)
import           Ouroboros.Consensus.Util.Orphans ()

{-------------------------------------------------------------------------------
  Orphan Instances

  The following instances are for types in the `cardano-ledger-specs` repo.
  These should eventually be moved to their respective packages.
-------------------------------------------------------------------------------}

{-------------------------------------------------------------------------------
  From Package: cardano-ledger-byron
-------------------------------------------------------------------------------}

instance ToCBOR Config where
  toCBOR
    (Config
      configGenesisData_
      configGenesisHash_
      configReqNetMagic_
      configUTxOConfiguration_
    ) = mconcat [
            encodeListLen 4
          , toCBOR @GenesisData configGenesisData_
          , toCBOR @GenesisHash configGenesisHash_
          , toCBOR @RequiresNetworkMagic configReqNetMagic_
          , toCBOR @UTxOConfiguration configUTxOConfiguration_
          ]

instance FromCBOR Config where
  fromCBOR = do
    enforceSize "Config" 4
    Config
      <$> fromCBOR @GenesisData
      <*> fromCBOR @GenesisHash
      <*> fromCBOR @RequiresNetworkMagic
      <*> fromCBOR @UTxOConfiguration


instance ToCBOR GenesisData where
  toCBOR
    (GenesisData
      gdGenesisKeyHashes_
      gdHeavyDelegation_
      gdStartTime_
      gdNonAvvmBalances_
      gdProtocolParameters_
      gdK_
      gdProtocolMagicId_
      gdAvvmDistr_
    ) = mconcat [
            encodeListLen 8
          , toCBOR @GenesisKeyHashes gdGenesisKeyHashes_
          , toCBOR @GenesisDelegation gdHeavyDelegation_
          , toCBOR {- @UTCTime -} gdStartTime_
          , toCBOR @GenesisNonAvvmBalances gdNonAvvmBalances_
          , toCBOR @ProtocolParameters gdProtocolParameters_
          , toCBOR @BlockCount gdK_
          , toCBOR @ProtocolMagicId gdProtocolMagicId_
          , toCBOR @GenesisAvvmBalances gdAvvmDistr_
          ]

instance FromCBOR GenesisData where
  fromCBOR = do
    enforceSize "GenesisData" 8
    GenesisData
      <$> fromCBOR @GenesisKeyHashes
      <*> fromCBOR @GenesisDelegation
      <*> fromCBOR -- @UTCTime
      <*> fromCBOR @GenesisNonAvvmBalances
      <*> fromCBOR @ProtocolParameters
      <*> fromCBOR @BlockCount
      <*> fromCBOR @ProtocolMagicId
      <*> fromCBOR @GenesisAvvmBalances

instance ToCBOR GenesisKeyHashes where
  toCBOR (GenesisKeyHashes gkh) = encodeListLen 1 <> toCBOR @(Set KeyHash) gkh

instance FromCBOR GenesisKeyHashes where
  fromCBOR = do
    enforceSize "GenesisKeyHashes" 1
    GenesisKeyHashes <$> fromCBOR @(Set KeyHash)

instance ToCBOR GenesisDelegation where
  toCBOR (UnsafeGenesisDelegation gd)
    = encodeListLen 1
      <> toCBOR @(Map KeyHash Certificate) gd

instance FromCBOR GenesisDelegation where
  fromCBOR = do
    enforceSize "GenesisDelegation" 1
    UnsafeGenesisDelegation <$> fromCBOR @(Map KeyHash Certificate)

instance ToCBOR GenesisNonAvvmBalances where
  toCBOR (GenesisNonAvvmBalances gnab)
    = encodeListLen 1
      <> toCBOR @(Map Address Lovelace) gnab

instance FromCBOR GenesisNonAvvmBalances where
  fromCBOR = do
    enforceSize "GenesisNonAvvmBalances" 1
    GenesisNonAvvmBalances <$> fromCBOR @(Map Address Lovelace)

instance ToCBOR GenesisAvvmBalances where
  toCBOR (GenesisAvvmBalances gab)
    = encodeListLen 1
      <> toCBOR @(Map CompactRedeemVerificationKey Lovelace) gab

instance FromCBOR GenesisAvvmBalances where
  fromCBOR = do
    enforceSize "GenesisAvvmBalances" 1
    GenesisAvvmBalances <$> fromCBOR @(Map CompactRedeemVerificationKey Lovelace)

instance ToCBOR UTxOConfiguration where
  toCBOR (UTxOConfiguration tcAssetLockedSrcAddrs_)
    = encodeListLen 1
    <> toCBOR @(Set CompactAddress) tcAssetLockedSrcAddrs_

instance FromCBOR UTxOConfiguration where
  fromCBOR = do
    enforceSize "UTxOConfiguration" 1
    UTxOConfiguration <$> fromCBOR @(Set CompactAddress)

{-------------------------------------------------------------------------------
  From Package: cardano-crypto-wrapper
-------------------------------------------------------------------------------}

instance ToCBOR CompactRedeemVerificationKey where
  toCBOR (CompactRedeemVerificationKey a b c d)
    = mconcat [
        encodeListLen 4
      , toCBOR @Word64 a
      , toCBOR @Word64 b
      , toCBOR @Word64 c
      , toCBOR @Word64 d
      ]

instance FromCBOR CompactRedeemVerificationKey where
  fromCBOR = do
    enforceSize "CompactRedeemVerificationKey" 1
    CompactRedeemVerificationKey
      <$> fromCBOR @Word64
      <*> fromCBOR @Word64
      <*> fromCBOR @Word64
      <*> fromCBOR @Word64

instance ToCBOR RequiresNetworkMagic where
  toCBOR = \case
    RequiresNoMagic -> encodeTag 0
    RequiresMagic   -> encodeTag 1

instance FromCBOR RequiresNetworkMagic where
  fromCBOR = decodeTag >>= \case
    0   -> return RequiresNoMagic
    1   -> return RequiresMagic
    tag -> fail $ "RequiresNetworkMagic: unknown tag " ++ show tag
