module DeltaQ.RationalProbabilityDoubleDelay
  ( module DQ
  , module DeltaQ.Algebra.DelayModel.SimpleUniform
  , DeltaQ
  )
where
import DeltaQ.Algebra hiding (DeltaQ)
import qualified DeltaQ.Algebra as DQ 
import DeltaQ.Algebra.DelayModel.SimpleUniform

type DeltaQ = DQ.DeltaQ Rational SimpleUniform Double
