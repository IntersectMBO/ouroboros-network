{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Mock key evolving signatures.
module Ouroboros.Consensus.Crypto.KES.Simple
    ( SimpleKES
    ) where

import           Codec.Serialise (Serialise(..))
import qualified Codec.Serialise.Encoding as Enc
import qualified Codec.Serialise.Decoding as Dec
import           Control.Monad (replicateM)
import           Data.Vector (Vector, fromList, (!?))
import qualified Data.Vector as Vec
import           GHC.Generics (Generic)
import           Numeric.Natural (Natural)

import           Ouroboros.Consensus.Crypto.DSIGN
import           Ouroboros.Consensus.Crypto.KES.Class
import           Ouroboros.Consensus.Util.Condense

data SimpleKES d

instance ( DSIGNAlgorithm d
           -- TODO We currently don't support other 'Signable' constraints for
           -- KES. We could, but it's more stuff to do. So for the moment we fix
           -- this here.
         , Signable d ~ Empty
         ) => KESAlgorithm (SimpleKES d) where

    newtype VerKeyKES (SimpleKES d) = VerKeySimpleKES (Vector (VerKeyDSIGN d))
        deriving Generic

    newtype SignKeyKES (SimpleKES d) =
        SignKeySimpleKES ([VerKeyDSIGN d], [(Natural, SignKeyDSIGN d)])
        deriving Generic

    newtype SigKES (SimpleKES d) = SigSimpleKES (SigDSIGN d)
        deriving Generic

    encodeVerKeyKES = encode
    encodeSignKeyKES = encode
    encodeSigKES = encode

    decodeSignKeyKES = decode
    decodeVerKeyKES = decode
    decodeSigKES = decode

    genKeyKES duration = do
        sks <- replicateM (fromIntegral duration) genKeyDSIGN
        let vks = map deriveVerKeyDSIGN sks
        return $ SignKeySimpleKES (vks, zip [0..] sks)

    deriveVerKeyKES (SignKeySimpleKES (vks, _)) = VerKeySimpleKES $ fromList vks

    signKES toEnc j a (SignKeySimpleKES (vks, xs)) = case dropWhile (\(k, _) -> k < j) xs of
        []           -> return Nothing
        (_, sk) : ys -> do
            sig <- signDSIGN toEnc a sk
            return $ Just (SigSimpleKES sig, SignKeySimpleKES (vks, ys))

    verifyKES toEnc (VerKeySimpleKES vks) j a (SigSimpleKES sig) =
        case vks !? fromIntegral j of
            Nothing -> False
            Just vk -> verifyDSIGN toEnc vk a sig

deriving instance DSIGNAlgorithm d => Show (VerKeyKES (SimpleKES d))
deriving instance DSIGNAlgorithm d => Eq (VerKeyKES (SimpleKES d))
deriving instance DSIGNAlgorithm d => Ord (VerKeyKES (SimpleKES d))
instance DSIGNAlgorithm d => Serialise (VerKeyKES (SimpleKES d)) where
  encode (VerKeySimpleKES vvks) =
       Enc.encodeListLen (fromIntegral $ Vec.length vvks)
    <> Vec.foldl' (<>) mempty (fmap encodeVerKeyDSIGN vvks)
  decode = VerKeySimpleKES <$> do
    len <- Dec.decodeListLen
    Vec.fromList <$> replicateM len decodeVerKeyDSIGN

deriving instance DSIGNAlgorithm d => Show (SignKeyKES (SimpleKES d))
deriving instance DSIGNAlgorithm d => Eq (SignKeyKES (SimpleKES d))
deriving instance DSIGNAlgorithm d => Ord (SignKeyKES (SimpleKES d))
instance DSIGNAlgorithm d => Serialise (SignKeyKES (SimpleKES d)) where
  encode (SignKeySimpleKES (vks, stuff)) =
       Enc.encodeListLen 2
    <> Enc.encodeListLen (fromIntegral $ length vks)
    <> mconcat (fmap encodeVerKeyDSIGN vks)
    <> Enc.encodeListLen (fromIntegral $ length stuff)
    <> mconcat (fmap encodeStuff stuff)
    where
      encodeStuff (n, skd) =
           Enc.encodeListLen 2
        <> Enc.encodeWord (fromIntegral n)
        <> encodeSignKeyDSIGN skd
  decode = SignKeySimpleKES <$> do
    Dec.decodeListLenOf 2
    vksLen <- Dec.decodeListLen
    vks <- replicateM vksLen decodeVerKeyDSIGN
    stuffLen <- Dec.decodeListLen
    stuff <- replicateM stuffLen decodeStuff
    return (vks, stuff)
    where
      decodeStuff = do
        Dec.decodeListLenOf 2
        n <- fromIntegral <$> Dec.decodeWord
        sks <- decodeSignKeyDSIGN
        return (n, sks)


deriving instance DSIGNAlgorithm d => Show (SigKES (SimpleKES d))
deriving instance DSIGNAlgorithm d => Eq (SigKES (SimpleKES d))
deriving instance DSIGNAlgorithm d => Ord (SigKES (SimpleKES d))
instance DSIGNAlgorithm d => Serialise (SigKES (SimpleKES d)) where
  encode (SigSimpleKES d) = encodeSigDSIGN d
  decode = SigSimpleKES <$> decodeSigDSIGN

instance Condense (SigDSIGN d) => Condense (SigKES (SimpleKES d)) where
    condense (SigSimpleKES sig) = condense sig
