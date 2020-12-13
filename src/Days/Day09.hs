
module Days.Day09  where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import Control.Applicative
import qualified Data.Set as Set
import Data.Vector (Vector)
import Control.Monad.Zip
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay)
import Data.Void
{- ORMOLU_ENABLE -}


------------ TYPES ------------
parse :: [String] -> [Int]
parse = fmap read

sumsOfPermutations l = nub  [x1 + x2 | x1 <- l, x2 <- l ]
move (_:tl) last = tl++[last]


partA list =
  let (queue, l) = splitAt 25 list
   in go queue l
  where
    go queue (hd : tl) =
      let sop = sumsOfPermutations queue
       in if hd `elem` sop
            then go (move queue hd) tl
            else hd

findContiguousSet invalid input =
  let solutionset = [ set | set <- contiguousSets input, sum set == invalid] in
    head solutionset

contiguousSets :: [Int] -> [[Int]]
contiguousSets [] = []
contiguousSets xs = filter (\xs -> length xs >=2) (inits xs) ++
                      contiguousSets (tail xs)

main = do
  input <- readFile "input/Day09.txt"
  let ls = lines input
      nums = parse ls
      num = partA nums
  print num
