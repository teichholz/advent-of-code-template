{-# LANGUAGE DeriveDataTypeable #-}
module Days.Day04 (runDay) where

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
import Data.Attoparsec.Text hiding (letter)
import Data.Attoparsec.Text as T hiding (letter)
import Control.Applicative (liftA2)
import Data.Void
import Data.Char (isAlpha, isDigit, isLower)
import Data.Data (toConstr, Data, Typeable, Constr)
import Control.Applicative.Combinators ( (<|>) )
import Data.Text (pack)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = passports

isLetter :: Char -> Bool
isLetter ':' = False
isLetter ' ' = False
isLetter '\n' = False
isLetter _ = True

letter :: Parser Char
letter = satisfy isLetter

pair :: Parser Pair
pair = do
  key <- many1 letter
  char ':'
  value <- many1 letter
  return $ tup2Pair key value

passport :: Parser Passport
passport = pair `sepBy1` (char ' ' <|> char '\n')

passports :: Parser [Passport]
passports = passport `sepBy1` string "\n\n"



-- Aux
tup2Pair :: String -> String -> Pair
tup2Pair "byr" str = Byr str
tup2Pair "iyr" str = Iyr str
tup2Pair "eyr" str = Eyr str
tup2Pair "hgt" str = Hgt str
tup2Pair "hcl" str = Hcl str
tup2Pair "ecl" str = Ecl str
tup2Pair "pid" str = Pid str
tup2Pair "cid" str = Cid str




------------ TYPES ------------
type Input = Passports

type Passports = [Passport]
type Passport = [Pair]

data Pair
  = Byr String
  | Iyr String
  | Eyr String
  | Hgt String
  | Hcl String
  | Ecl String
  | Pid String
  | Cid String
  deriving (Show, Data, Typeable)

type OutputA = Int

type OutputB = Int

optional :: Constr
optional = toConstr (Cid "")

filterOptional :: Passport -> Passport
filterOptional = filter ((not . (==optional)) . toConstr)

validPassports :: Passports -> Passports
validPassports = filter ((==7) . length) . fmap filterOptional

checkConstraints :: Pair -> Bool
checkConstraints (Byr str) = length str == 4 && inRange str 1920 2002
checkConstraints (Iyr str) = length str == 4 && inRange str 2010 2020
checkConstraints (Eyr str) = length str == 4 && inRange str 2020 2030
checkConstraints (Hgt str) = validHgt str
checkConstraints (Hcl str) = validHcl str
checkConstraints (Ecl str) = str `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
checkConstraints (Pid str) = length str == 9 && and (isDigit <$> str)
checkConstraints _ = False

validConstraints :: Passports -> Passports
validConstraints = filter (and . fmap checkConstraints)

inRange :: String -> Int -> Int -> Bool
inRange str lower higher = (liftA2 (&&) (>=lower) (<=higher)) $ read str
validHgt (x : y : z : "cm") = inRange [x,y,z] 150 193
validHgt (x : y : "in") = inRange [x,y] 59 76
validHgt _ = False
validHcl ('#' : cl) = length cl == 6 && and (liftA2 (||) isDigit isLower <$> cl)
validHcl _ = False


------------ PART A ------------
partA :: Input -> OutputA
partA = length . validPassports

------------ PART B ------------
partB :: Input -> OutputB
partB = length . validConstraints . validPassports
