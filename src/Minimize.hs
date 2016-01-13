{-
 This executable will parse a dot file to a mealy machine (and you can
 specify the initial state). Then it minimizes the machine and writes
 it as dot again. All I/O is through stdin and stdout. It is fairly
 efficient: The ESM case can be minimized in 22 seconds (it is a Mealy
 machine with 3410 states and 78 input).
-}

import           Data.ByteString         (getContents)
import           Data.ByteString.Builder (hPutBuilder)
import           Mealy
import           Prelude                 hiding (getContents)
import           System.Environment      (getArgs)
import           System.Exit             (exitFailure)
import           System.IO               (hPrint, stderr, stdout)

main :: IO ()
main = do
  initialGuessStrategy <- getInitialGuess <$> getArgs
  p <- parseMealy initialGuessStrategy <$> getContents
  case p of
    Left e -> hPrint stderr e >> exitFailure
    Right mealy -> hPutBuilder stdout . writeMealyToDot . minimizeMealy $ mealy
  where
    getInitialGuess [] = FirstEdge
    getInitialGuess [x] = read x
    getInitialGuess [x,y] = Constant y
