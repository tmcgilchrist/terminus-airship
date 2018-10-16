{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Terminus.Routes where

import           Airship (RoutingSpec, resourceToWai, defaultAirshipConfig, (#>), star)
import           Control.Concurrent.Async (async, wait, cancel)
import           Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString.Lazy as BSL
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Network.HTTP.Types (methodGet, status200)
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp (defaultSettings,
                                           runSettings, setHost,
                                            setPort)
import           Data.Semigroup ((<>))
import qualified Network.Wai.Test as WT
import           Prelude
import           Terminus (healthResource, errors)

import           Network.HTTP.Client

testRoute :: RoutingSpec IO ()
testRoute = do
  "health" #> healthResource

prop_endpoint_create :: Property
prop_endpoint_create = withTests 1 . property $ do

  let port = 3000
      host = "127.0.0.1"
      req = "http://127.0.0.1:3000/health"
      settings = setPort port (setHost host defaultSettings)

  let app = resourceToWai defaultAirshipConfig testRoute errors
  a <- liftIO . async $ runSettings settings app

  let settings = managerSetProxy
            (proxyEnvironment Nothing)
            defaultManagerSettings
  man <- liftIO $ newManager settings

  r <- liftIO $ httpLbs req man
  responseStatus r === status200

tests :: IO Bool
tests = checkSequential $$(discover)
