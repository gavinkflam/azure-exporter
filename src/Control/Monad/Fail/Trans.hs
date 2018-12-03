module Control.Monad.Fail.Trans
    (
      -- * Either
      failLeft
      -- * Maybe
    , failNothing
    ) where

import Control.Monad.Fail (MonadFail)

-- | Fail with error message if `Left`, otherwise extract the `Right` value.
failLeft :: MonadFail m => Either String a -> m a
failLeft (Left m)  = fail m
failLeft (Right x) = return x

-- | Fail with error message if `Nothing`, otherwise extract the `Just` value.
failNothing :: MonadFail m => String -> Maybe a -> m a
failNothing e Nothing  = fail e
failNothing _ (Just x) = return x
