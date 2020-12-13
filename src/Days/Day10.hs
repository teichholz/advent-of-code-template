module Days.Day10 where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import Data.Function ((&))
import Control.Applicative (liftA2)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import Data.Void
{- ORMOLU_ENABLE -}

partA nums = diffs nums & filter (liftA2 (||) (==3) (==1)) & (3:) & sort & group & fmap length & product

diffs nums = zipWith (-) (tail nums) nums
sums nums = zipWith (+) (tail nums) nums
-- TODO part b
-- [1 1 1 1 3 2 1 3 1 1 1]
split ele nums@(hd:tl)
  | ele == hd =
    if null tl then [] else split ele tl
  | otherwise =
    let (f, s) = span (/= ele) nums in
      if null s then [f] else f:split ele s

combinations diffs =
  1 + go diffs 3
  where go (hd:tl) max
          | null tl = 0
          | hd < max = 1 + go tl (max - hd) + go tl max
          | otherwise = go tl max

partB = fmap combinations . split 3 . diffs

main = do
  input <- readFile "input/Day10.txt"
  let ls = lines input
      nums = fmap read ls :: [Int]
      sorted = sort nums
    in do
      print $ product $ partB (0:sorted)
      print $ partB sorted
      print $ split 3 $ diffs sorted
      print $ diffs sorted
      print sorted
