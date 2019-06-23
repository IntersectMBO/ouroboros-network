module Main
where

import DeltaQ.RationalProbabilityDoubleDelay
import DeltaQ.Algebra.Simplification
import DeltaQ.QTA.Support

main :: IO ()
main = do
  print qta1 >> print (mmmv qta1) >> print (tangibleMass qta1)
  print qta2 >> print (mmmv qta2) >> print (tangibleMass qta2)
  let qta2' = canonicaliseDeltaQ qta2
  print qta2' >> print (mmmv qta2') >> print (tangibleMass qta2)
  print (Just qta1 == tangibleRandomness qta2')

intent :: [(Rational, Double)]
intent = [(50/100,1), (75/100,2), (90/100,3), (95/100,4), (100/100,5)]    

qta1 :: DeltaQ
qta1 = fromQTA intent

qta2 :: DeltaQ
qta2 = (⇋) (95/100) qta1 (⊥)
