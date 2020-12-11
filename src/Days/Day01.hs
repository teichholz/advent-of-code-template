module Days.Day01 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U
import Control.Monad

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text as T
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = ints

int :: Parser Int
int = read <$> many1 digit
ints :: Parser [Int]
ints = int `sepBy` char '\n'

------------ TYPES ------------
type Input = [Int]

type OutputA = Integer

type OutputB = Void

------------ PART A ------------
partA :: Input -> Int
partA input =
  let myMap = Map.fromList $ (\x -> (x,x)) <$> input in
  head $ catMaybes $ input >>= (\inEle -> [Map.lookup (2020-inEle) myMap >>= (\to2020 -> Just $ to2020*inEle)]) 




------------ PART B ------------
partB =
  head . catMaybes . f
  where
    f input = do
      x <- input
      y <- input
      z <- input
      if is2020 $ x + y + z then return (Just $ x * y * z) else return Nothing

is2020 = (== 2020)

findProduct n = product . head . filter ((== 2020) . sum) . replicateM n

go nums = forM_ [2,3] (print . (`findProduct` nums))
