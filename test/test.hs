import           Control.Monad (unless)

import           System.Exit (exitFailure)
import           System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import qualified Test.IO.Terminus.Routes
import qualified Test.IO.Terminus.Other


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence [
      Test.IO.Terminus.Routes.tests
    , Test.IO.Terminus.Other.tests
    ]

  unless (and results) System.Exit.exitFailure
  pure ()
