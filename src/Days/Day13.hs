module Days.Day13 where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Function ((&))
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

{- ORMOLU_ENABLE -}
parse :: [String] -> (Int, [Int])
parse [num, nums] =
  let f = read num
      s = split ',' nums in
    (f, read <$> filter (/="x") s)

split ele nums@(hd:tl)
  | ele == hd =
    if null tl then [] else split ele tl
  | otherwise =
    let (f, s) = span (/= ele) nums in
      if null s then [f] else f:split ele s

seqs nums = fmap (\hi -> [0,hi..]) nums
seqsAt at seqs = take 1000 . dropWhile (<at) <$> seqs
minOfMins seqs = minimum $ fmap minimum seqs
earliestDeparture num nums = seqs nums & seqsAt num & minOfMins
isBusId min num = 0 == min `mod` num
busId min nums = filter (isBusId min) nums  & head
partA num nums =
  let earliest = earliestDeparture num nums
      diff = earliest - num
      id = busId earliest nums in
    (earliest, diff, id)

main = do
  content <- readFile "input/Day13.txt"
  let ls = lines content
      (num, nums) = parse ls
  print $ partA num nums
