
module Ouroboros.Network.Util.PrettyShow (PrettyShow (..)) where

import Network.Socket (SockAddr (..))

-- | Pretty print a data type.
--
-- Note: `PrettyShow` should not be used when one wants to rely on
-- compatibility between `Show` and `Read` instances or be able to copy paste
-- code (e.g. for `Arbitrary` generators).
--
-- It carries `Show` constraint, which simplifies type signatures.  `Show` is
-- anyway required by `Exception` type class, so we cannot simply replace
-- `Show` by `PrettyShow`.
--
class Show a => PrettyShow a where
  prettyShow :: a -> String
  prettyShow = show

instance PrettyShow Int where
instance PrettyShow Word where
instance PrettyShow SockAddr where
