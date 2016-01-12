import           Data.ByteString  (getContents)
import           Data.Map.Strict  as M
import           Data.Maybe       (fromJust)
import           Data.Set         as S
import           Data.Time.Clock  (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Mealy
import           Minimization
import           Prelude          hiding (getContents)

main :: IO ()
main = do
  printStampd "Hello"
  input <- getContents
  let p = parseMealy input
  case p of
    Left e -> printStampd e
    Right _ -> printStampd "successfully parsed"
  let Right mealy = p
  printStampd (M.size mealy)
  let states = keys mealy
  let inputs = S.toList . M.foldr S.union S.empty $ fmap keysSet mealy
  let beh s i = ulookup i (ulookup s mealy)
  let outputs s = fmap (snd . beh s) inputs
  let transitions = fmap (\i s -> fst $ beh s i) inputs
  let mini = minimize states outputs transitions
  printStampd $ Minimization.size mini
  where
    ulookup k m = fromJust $ M.lookup k m
    printTime time = putStr $ formatTime defaultTimeLocale "%T: " time
    printNow = getCurrentTime >>= printTime
    printStampd x = seq x $ printNow >> print x
