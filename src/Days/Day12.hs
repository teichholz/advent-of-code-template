module Days.Day12  where

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
import Control.Monad.State.Lazy
import Data.Bifunctor (bimap)

import Data.Void
{- ORMOLU_ENABLE -}

type Direction = Pos
type Pos = (Float, Float)
type ST = (Direction, Pos)

parse ('L':num) = (deg2pos $ (read num :: Float), 0)
parse ('R':num) = (deg2pos $ 360 - (read num :: Float), 0)
parse ('N':num) = ((0, 1), read num)
parse ('S':num) = ((0, -1), read num)
parse ('E':num) = ((1, 0), read num)
parse ('W':num) = ((-1, 0), read num)
parse ('F':num) = ((0, 0), read num)

deg2pos deg = (cos $ deg2rad deg, sin $ deg2rad deg)
deg2rad = (*) (pi / 180)

main = do
  input <- readFile "input/Day12.txt"
  let ls = lines input
      ops = fmap parse ls
  print ls
  print ops
