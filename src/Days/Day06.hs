module Days.Day06 where

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

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}


groupLs = groupBy (\_ a2 -> not . null $ a2) . lines

groupStrs = fmap concat . groupLs

allYes = foldl1 intersect . (filter (not . (=="")))

sumOfllength = sum . fmap length

main = do
    input <- readFile "input/Day06.txt"
    print $ sumOfllength $ fmap nub $ groupStrs input
    print $ sumOfllength $ fmap allYes $ groupLs input
