import           Data.ByteString         (getContents)
import           Data.ByteString.Builder (hPutBuilder)
import           Mealy
import           Prelude                 hiding (getContents)
import           System.Exit             (exitFailure)
import           System.IO               (hPrint, stderr, stdout)

main :: IO ()
main = do
  p <- parseMealy <$> getContents
  case p of
    Left e -> hPrint stderr e >> exitFailure
    Right mealy -> hPutBuilder stdout . writeMealyToDot . minimizeMealy $ mealy
