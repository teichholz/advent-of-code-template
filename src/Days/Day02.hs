module Days.Day02 (runDay) where

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
import Control.Applicative

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = juxt `sepBy` endOfLine

int :: Parser Int
int = read <$> many1 digit

policy :: Parser Policy
policy = do
  min <- int
  skip (=='-')
  max <- int
  skipSpace
  letter <- letter
  char ':'
  skipSpace
  return (min, max, letter)

password :: Parser Password
password = many1 letter

juxt :: Parser Juxt
juxt = liftA2 (,) policy password

------------ TYPES ------------
type Input = [Juxt]

type Policy = (Int, Int, Char)
type Password = String
type Juxt = (Policy, Password)

type OutputA = Int

type OutputB = Int

-- Aux

validJuxt :: (Policy -> Password -> Bool) -> Juxt -> Bool
validJuxt isValid (policy, password) = isValid policy password

------------ PART A ------------
partA :: Input -> OutputA
partA = length . filter (validJuxt isValidA)

isValidA :: Policy -> Password -> Bool
isValidA (min, max, char) =
  (liftA2 (&&) (>= min) (<= max)) . length . filter (== char)



------------ PART B ------------
partB :: Input -> OutputB
partB = length . filter (validJuxt isValidB)

isValidB :: Policy -> Password -> Bool
isValidB (i, k, char) password =
  (liftA2 (/=) (==password!!(i-1)) (==password!!(k-1))) char
