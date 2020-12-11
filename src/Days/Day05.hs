{-# LANGUAGE OverloadedStrings #-}
module Days.Day05 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Bifunctor (bimap)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U


import Control.Applicative.Combinators ( (<|>) )
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Control.Applicative (liftA2)
import Data.Monoid
import Control.Monad.Trans.State.Lazy
import Control.Arrow ((***), first)
import Data.Attoparsec.Text as T hiding (letter)
import Data.Text (pack)
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = boardingpasses

opify :: Char -> Op
opify 'L' = left
opify 'R' = right
opify 'F' = up
opify 'B' = down

row = do
  char <- count 7 anyChar
  return $ opify <$> char

col = do
  char <- count 3 anyChar
  return $ opify <$> char

boardingpass :: Parser Boardingpass
boardingpass = liftA2 (,) row col

boardingpasses :: Parser Boardingpasses
boardingpasses = (boardingpass `sepBy1` endOfLine) <* endOfLine

-- Aux
up (value, status) = (value, status * 2)
down (value, status) = (value + status, status * 2)
left = up
right = down

unify :: [Op] -> Op
unify ops = appEndo $ mconcat $ Endo <$> ops
unifyPass :: ([Op], [Op]) -> (Op, Op)
unifyPass (row, col) = (unify row, unify col)

solve pass =
  let (f, g) = unifyPass pass in
    (fst $ f (0, 1)) * 8 + (fst $ g (0, 1))

findSeat :: [Int] -> Int
findSeat seats =
    let seats' = zip [(minimum seats) ..] (sort seats)
    in  fst . head $ dropWhile (uncurry (==)) seats'

------------ TYPES ------------
type Input = Boardingpasses
type Op = (Int, Int) -> (Int, Int)
type Row = [Op]
type Col = [Op]
type Boardingpass = (Row, Col)
type Boardingpasses = [Boardingpass]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA = maximum . fmap solve

------------ PART B ------------
partB :: Input -> OutputB
partB input = findSeat seats
  where seats = fmap solve input



one :: State (Int, Int) ()
one = modify (\(v, s) -> (v+s, s * 2))
zero :: State (Int, Int) ()
zero = modify (\(v, s) -> (v, s * 2))

mapC 'B' = one
mapC 'F' = zero
mapC 'R' = one
mapC 'L' = zero

run line = fst $ execState (traverse mapC (reverse line)) (0, 1)
seatIds line = uncurry (+) $ first (*8) $ bimap run run $ splitAt 7 line

myID ids@(id:_) = snd . head $ dropWhile (uncurry (==)) $ zip ids [id..]

day5 :: IO ()
day5 = do
    input <- readFile "input/Day05.txt"
    let ids = seatIds <$> lines input
    print $ maximum ids
    print $ myID $ sort ids
