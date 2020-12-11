module Days.Day08 where
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Applicative (liftA2)
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Control.Monad.Trans.State.Lazy
import qualified Util.Util as U
import Data.Bifunctor (bimap)

import qualified Program.RunDay as R (runDay)
import Data.Void
{- ORMOLU_ENABLE -}

type Input = Void

type Op = String
type Argument = Int
type Seen = Vector Bool
type Instr = (Op, Argument)
type Program = [Instr]

type OutputA = Void

type OutputB = Void

parse :: [String] -> [Instr]
parse = fmap $ makeInstr . words
makeInstr [op, arg] = (op, makeArg arg)
makeArg ('+':num) = read num
makeArg num = read num

makeSeen prog = Vec.replicate (length prog) False
setSeen acc seen = Vec.update seen (Vec.singleton (acc, True))

execute :: Program -> Int -> State (Int, Vector Bool) Int
execute program pc = do
  (acc, seen) <- get
  if seen Vec.! pc then
    return acc
  else
    case program!!pc of
       ("acc", arg) -> do
         modify (bimap (+ arg) (setSeen pc))
         execute program (pc + 1)
       ("jmp", arg) -> do
         modify (bimap id (setSeen pc))
         execute program (pc + arg)
       ("nop", _) -> do
         modify (bimap id (setSeen pc))
         execute program (pc + 1)


main = do
  input <- readFile "input/Day08.txt"
  let ls = lines input in
    let prog = parse ls in
    print $ evalState (execute prog 0) (0, makeSeen prog)

