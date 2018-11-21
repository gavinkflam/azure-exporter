-- | Combinators for HUnit.
module Expectations
    (
      -- * Predicates
      isJustOf
    , isLeftOf
    , isRightOf
    ) where

-- | Return `True` if the given value is a `Just` value of the given value,
--   `False` otherwise.
isJustOf :: Eq a => a -> Maybe a -> Bool
isJustOf exp (Just v) = exp == v
isJustOf _ Nothing    = False

-- | Return `True` if the given value is a `Left` value of the given value,
--   `False` otherwise.
isLeftOf :: Eq a => a -> Either a b -> Bool
isLeftOf exp (Left v) = exp == v
isLeftOf _ (Right _)  = False

-- | Return `True` if the given value is a `Right` value of the given value,
--   `False` otherwise.
isRightOf :: Eq b => b -> Either a b -> Bool
isRightOf exp (Right v) = exp == v
isRightOf _ (Left _)    = False
