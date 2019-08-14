{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Util.SOP (
    constrName
  , constrNames
  ) where

import           Data.Proxy
import qualified Generics.SOP as SOP

constrInfo :: SOP.HasDatatypeInfo a
           => proxy a
           -> SOP.NP SOP.ConstructorInfo (SOP.Code a)
constrInfo = SOP.constructorInfo . SOP.datatypeInfo

constrName :: forall a. SOP.HasDatatypeInfo a => a -> String
constrName a =
    SOP.hcollapse $ SOP.hliftA2 go (constrInfo p) (SOP.unSOP (SOP.from a))
  where
    go :: SOP.ConstructorInfo b -> SOP.NP SOP.I b -> SOP.K String b
    go nfo _ = SOP.K $ SOP.constructorName nfo

    p = Proxy @a

constrNames :: SOP.HasDatatypeInfo a => proxy a -> [String]
constrNames p =
    SOP.hcollapse $ SOP.hmap go (constrInfo p)
  where
    go :: SOP.ConstructorInfo a -> SOP.K String a
    go nfo = SOP.K $ SOP.constructorName nfo
