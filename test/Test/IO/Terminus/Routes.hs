{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Terminus.Routes where

import           Airship (RoutingSpec, resourceToWai, defaultAirshipConfig, (#>))
import           Control.Concurrent.Async (async)
import           Control.Monad.IO.Class (MonadIO(..))
import           Hedgehog
import           Network.HTTP.Types (status200)
import           Network.Wai.Handler.Warp (defaultSettings,
                                           runSettings, setHost,
                                            setPort)
import           Prelude
import           Terminus (healthResource, errors)

import           Network.HTTP.Client (managerSetProxy, newManager, httpLbs
                                     , responseStatus, defaultManagerSettings, proxyEnvironment)

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
  _ <- liftIO . async $ runSettings settings app

  let settings' = managerSetProxy
            (proxyEnvironment Nothing)
            defaultManagerSettings
  man <- liftIO $ newManager settings'

  r <- liftIO $ httpLbs req man
  responseStatus r === status200

tests :: IO Bool
tests = checkSequential $$(discover)
