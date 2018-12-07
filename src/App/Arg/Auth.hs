module App.Arg.Auth
    (
      -- * Arguments
      assembleArgs
    ) where

import Control.Concurrent.STM.TVar (TVar)

import Control.Lens ((^.))
import Network.HTTP.Client (Manager)

import qualified Data.App.AccessToken as At
import qualified Data.App.AppEnv as En
import qualified Data.App.Config as Cf

-- | Assemble the arguments for refreshing access token.
assembleArgs :: En.AppEnv -> (Manager, Cf.Config, TVar At.AccessToken)
assembleArgs appEnv =
    ( appEnv ^. En.httpManager
    , appEnv ^. En.config
    , appEnv ^. En.accessToken
    )
