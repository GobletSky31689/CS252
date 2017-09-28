import Data.Map (Map)
import qualified Data.Map as Map

m = Map.empty

m' = Map.insert "a" 42 m

main = do
  case (Map.lookup "a" m') of
    Just i -> putStrLn $ show i
    _      -> error "Key is not in the map"

