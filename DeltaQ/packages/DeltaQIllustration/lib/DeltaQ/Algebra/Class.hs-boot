module DeltaQ.Algebra.Class
  ( ImproperRandomVar(..)
  )
where

class ImproperRandomVar m where
    tangibleMass       :: m -> m
    tangibleRandomness :: m -> Maybe m
