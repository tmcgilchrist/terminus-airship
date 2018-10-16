{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Terminus.Other where

import           Airship
import           Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString.Lazy as BSL
import           Hedgehog
import           Network.HTTP.Types
import           Network.Wai (Application)
import qualified Network.Wai.Test as WT
import           Prelude
import           Terminus (healthResource, errors)

makeRequest :: Application -> Request -> IO WT.SResponse
makeRequest a r = do
  x <- BSL.fromStrict <$> requestBody r
  WT.runSession (WT.srequest $ WT.SRequest r x) a

testRoute :: RoutingSpec IO ()
testRoute = do
  "health" #> healthResource

prop_endpoint_create :: Property
prop_endpoint_create = withTests 1 . property $ do
  let req = WT.defaultRequest {
          requestMethod = methodGet
        , pathInfo = ["health"]
        }

  let app = resourceToWai defaultAirshipConfig testRoute errors
  r <- liftIO $ makeRequest app req
  WT.simpleStatus r === status200

tests :: IO Bool
tests = checkSequential $$(discover)
