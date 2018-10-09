-- |
--
-- Combinators for HUnit.
module Expectations
  -- * Predicates
  -- To be used with `shouldSatisfy`.
  ( isLeftOf
  ) where

-- |
-- Return `True` if the given value is a `Left` value of the given value,
-- `False` otherwise.
isLeftOf :: Eq a => a -> Either a b -> Bool
isLeftOf exp (Left v) = exp == v
isLeftOf _ (Right _)  = False
