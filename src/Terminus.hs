{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Terminus (
    routes
  , errors
  ) where

import           Airship
import qualified Data.Map.Strict as M
import           Network.HTTP.Media (MediaType)
import qualified Network.HTTP.Types as HTTP
import           Protolude

routes :: Resource IO -> RoutingSpec IO ()
routes static = do
  "health" #> healthResource
  var "word" #> wordResource
  star #> static

errors :: M.Map HTTP.Status [(MediaType, Webmachine IO ResponseBody)]
errors = let response404 = escapedResponse "<html><head></head><body><h1>404 Not Found</h1></body></html>"
         in M.singleton HTTP.status404 [("text/html", return response404)]

healthResource :: Resource IO
healthResource = defaultResource
  { allowedMethods = return [HTTP.methodGet]
  , contentTypesProvided = return [("text/html", healthResponse)]
  }

healthResponse :: Webmachine IO ResponseBody
healthResponse = pure . escapedResponse $ "UP"

wordResource :: Resource IO
wordResource = defaultResource {
    allowedMethods = return [HTTP.methodGet]
  , contentTypesProvided = return [("text/html", wordResponse)]
}

wordResponse :: Webmachine IO ResponseBody
wordResponse = do
  w <- lookupParam "word"
  pure . escapedResponse $  mconcat ["<h1>Scotty, ", w, " me up!</h1>"]
