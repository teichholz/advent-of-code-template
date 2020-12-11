module Days.Day03 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import Control.Applicative.Combinators ( (<|>) )
import qualified Data.Set as Set
import Data.Vector (Vector)
import Control.Monad.Trans.State
import qualified Data.Vector as Vec
import Data.Array as Arr
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay)
import Data.Text (pack)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = biome

main = do
  string <- readFile "input/Day03.txt"
  print $ feed (parse biome (pack string)) ""
  return ()

node :: Parser Node
node =  (const Tree <$> char '#') <|> (const Square <$> char '.')

row :: Parser [Node]
row = many1 node

rows :: Parser [[Node]]
rows = row `sepBy` endOfLine <* endOfLine

mkAssoc ::  [[Node]] -> ((Int, Int), [Node])
mkAssoc rows =(dim, [node | row <- rows, node <- row])
  where dim = ((length rows) - 1, (length $ head rows) - 1)

biome :: Parser Biome
biome = (\(dim, assoc) -> listArray ((0,0), dim) assoc) <$> mkAssoc <$> rows

------------ TYPES ------------
type Input = Biome
type Biome = Array (Int, Int) Node
data Node = Tree | Square deriving (Show, Enum, Eq)
type Pos = (Int, Int)

type OutputA = Int

type OutputB = Int


-- Aux
(!>) array (y, x) = array ! (y , x `mod` (width array + 1))

width :: Array (Int, Int) a -> Int
width = snd . snd . bounds

height :: Array (Int, Int) a -> Int
height = fst . snd . bounds

move :: Pos -> Pos -> Pos
move (y, x) (yp, xp) = (y+yp, x+xp)

nodeToInt :: Node -> Int
nodeToInt Tree = 1
nodeToInt Square = 0

solve :: Pos -> Biome -> Int
solve moveTo biome =
  let h = height biome in go (0,0) moveTo biome 0 h
  where
    go pos@(y, _) moveTo biome sum h =
      if y <= h
        then go (move pos moveTo) moveTo biome (sum + nodeToInt (biome !> pos)) h
        else sum

------------ PART A ------------
partA :: Input -> OutputA
partA = solve (1, 3)


------------ PART B ------------
partB :: Input -> OutputB
partB input =
  let moves = [(1,1), (1,3), (1,5), (1,7), (2, 1)] in
    product $ (flip solve) input <$> moves
