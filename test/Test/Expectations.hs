-- | Combinators for HUnit.
module Test.Expectations
    (
      -- * Predicates
      isJustOf
    , isLeftOf
    , isRightOf
    ) where

-- | Return `True` if the given value is a `Just` value of the given value,
--   `False` otherwise.
isJustOf :: Eq a => a -> Maybe a -> Bool
isJustOf x (Just v) = x == v
isJustOf _ Nothing  = False

-- | Return `True` if the given value is a `Left` value of the given value,
--   `False` otherwise.
isLeftOf :: Eq a => a -> Either a b -> Bool
isLeftOf x (Left v)  = x == v
isLeftOf _ (Right _) = False

-- | Return `True` if the given value is a `Right` value of the given value,
--   `False` otherwise.
isRightOf :: Eq b => b -> Either a b -> Bool
isRightOf x (Right v) = x == v
isRightOf _ (Left _)  = False
