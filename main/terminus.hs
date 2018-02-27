{-# LANGUAGE OverloadedStrings #-}

import           Airship
import           Airship.Resource.Static

import           Network.Wai.Handler.Warp (defaultSettings, runSettings,
                                           setHost, setPort)
import           System.IO

import           Terminus

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  static <- staticResource Cache "assets"

  let port = 3000
      host = "0.0.0.0"
      settings = setPort port (setHost host defaultSettings)
      routes' = routes static

  putStrLn "Listening on port 3000"

  runSettings settings (resourceToWai defaultAirshipConfig routes' errors)
