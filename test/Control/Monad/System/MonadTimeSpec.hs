-- | Test environment variables reading monad.
module Control.Monad.System.MonadTimeSpec
    (
      -- * Spec
      spec
    ) where

import Data.Time.Clock (UTCTime)
import Data.Time.Clock.System (SystemTime(..), systemToUTCTime)

import Test.Hspec

import Control.Monad.System.MonadTime (MonadTime(..), runPure)

-- | Spec for `MonadTime`.
spec :: Spec
spec =
    describe "runPure" $
        it "returns the expected time" $
            runPure testTime testSeq `shouldBe` systemToUTCTime testTime

-- | Test sequence to derive system time in UTCTime.
testSeq :: MonadTime m => m UTCTime
testSeq = systemToUTCTime <$> getSystemTime

-- | Test time to mock system time.
testTime :: SystemTime
testTime = MkSystemTime
    { systemSeconds     = 1544176181
    , systemNanoseconds = 439304054
    }
