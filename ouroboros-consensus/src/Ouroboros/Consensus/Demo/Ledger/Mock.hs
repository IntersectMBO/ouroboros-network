{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ouroboros.Consensus.Demo.Ledger.Mock () where

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise
import           Data.Typeable (Typeable)

import           Ouroboros.Consensus.Crypto.Hash
import           Ouroboros.Consensus.Demo.Run
import           Ouroboros.Consensus.Ledger.Abstract
import           Ouroboros.Consensus.Ledger.Mock
import           Ouroboros.Consensus.Protocol.Abstract
import           Ouroboros.Consensus.Util.Condense

{-------------------------------------------------------------------------------
  RunDemo instance for the mock ledger
-------------------------------------------------------------------------------}

instance HashAlgorithm h => DemoHeaderHash (Hash h a) where
  demoEncodeHeaderHash = Serialise.encode
  demoDecodeHeaderHash = Serialise.decode

instance ( SupportedBlock (BlockProtocol (SimpleHeader SimpleMockCrypto ext))
                          (SimpleHeader SimpleMockCrypto ext)
         , Condense ext
         , Typeable ext
         , Serialise ext
         ) => DemoHeader (SimpleHeader SimpleMockCrypto ext) where
  demoEncodeHeader   = const Serialise.encode
  demoDecodeHeader   = const Serialise.decode
  demoBlockFetchSize = simpleBlockSize . simpleHeaderStd

instance ( ProtocolLedgerView (SimpleBlock SimpleMockCrypto ext)
         , Typeable ext
         , Condense ext
         , Serialise ext
         ) => DemoBlock (SimpleBlock SimpleMockCrypto ext) where
  demoEncodeBlock = const Serialise.encode
  demoDecodeBlock = const Serialise.decode
  demoMockTx      = \_ -> SimpleGenTx

instance ( SupportedBlock (BlockProtocol (SimpleHeader SimpleMockCrypto ext))
                                         (SimpleHeader SimpleMockCrypto ext)
         , ProtocolLedgerView (SimpleBlock  SimpleMockCrypto ext)
         , Condense ext
         , Typeable ext
         , Serialise ext
         ,   BlockProtocol (SimpleBlock  SimpleMockCrypto ext)
           ~ BlockProtocol (SimpleHeader SimpleMockCrypto ext)
         , ForgeExt (BlockProtocol (SimpleHeader SimpleMockCrypto ext))
                    SimpleMockCrypto
                    ext
         ) => RunDemo (SimpleBlock  SimpleMockCrypto ext)
                      (SimpleHeader SimpleMockCrypto ext) where
  demoForgeBlock         = forgeSimple
  demoGetHeader          = simpleHeader
  demoBlockMatchesHeader = matchesSimpleHeader
