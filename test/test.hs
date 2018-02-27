import           Control.Monad (unless)

import           System.Exit (exitFailure)
import           System.IO (BufferMode (..), hSetBuffering, stderr, stdout)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  results <- sequence [
    ]

  unless (and results) System.Exit.exitFailure
  pure ()
