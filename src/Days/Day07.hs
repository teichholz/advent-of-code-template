module Days.Day07 where

import Data.Map hiding (map, filter)
import Data.List.Split

type Node = String
type Graph = Map Node [(Node, Int)]

parse :: String -> (Node,[(Node,Int)])
parse s = let [s1,s2] = splitWhen (== "contain") $ words s
              col = foldl1 (\x y -> x++" "++y) $ init s1
              s3 = splitWhen (\s -> s == "bags," || s == "bags."
                                 || s == "bag."  || s == "bag,") s2
              toNode (x:xs) = (foldl1 (\a b -> a++" "++b) xs, (read x) :: Int)
          in  (col, if head s2 == "no" then [] else map toNode $ init s3)

trav :: Graph -> Node -> [Node]
trav g s = s : (concatMap (trav g) (map fst ch))
  where ch = findWithDefault [] s g

countBags :: Graph -> Node -> Int
countBags g s = sum (map snd ch) + sum (map (\(c,n) -> n * countBags g c) ch)
  where ch = findWithDefault [] s g

sol = do rules <- lines <$> readFile "input/Day07.txt"
         let g = fromList $ map parse rules
             haveGold = filter (elem "shiny gold") $ map (trav g) (keys g)
         return $ (length haveGold - 1, countBags g "shiny gold")
