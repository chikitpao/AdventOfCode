-- Day06.hs
-- AoC 2019 Day 6: Universal Orbit Map
-- Author: Chi-Kit Pao

--import Data.List
import Data.List.Split (splitOn)
import Data.Maybe ( fromJust )
import qualified Data.Map as Map
import qualified Data.Set as Set

swap (x,y) = (y,x)
tuplify2 [x,y] = (x,y)

countOrbit :: Map.Map [Char] [Char] -> [Char] -> Int
countOrbit m s =
    case Map.lookup s m of
        Nothing -> 0
        Just p -> countOrbit m p + 1

getParentList :: Map.Map [Char] [Char] -> [Char] -> [[Char]]
getParentList m s =
    case Map.lookup s m of
        Nothing -> [s]
        Just p -> getParentList m p ++ [s]

countTransfer :: Map.Map [Char] [Char] -> [Char] -> [Char] -> Int
countTransfer m obj1 obj2 = 
    let parents1 = getParentList m obj1
        parents2 = getParentList m obj2
        commonParents = takeWhile (uncurry (==)) $ zip parents1 parents2 in
    length parents1 + length parents2 - 2 * (length commonParents + 1)

main :: IO ()
main = do
    inputLines <- lines <$> readFile "input.txt"
    let ml = [ swap $ tuplify2 $ splitOn ")" s | s <- inputLines]
        m = Map.fromList ml

    putStrLn "Question 1: What is the total number of direct and indirect orbits in your map data?"
    print $ sum [countOrbit m a | (a, b) <- ml]

    putStrLn "Question 2: What is the minimum number of orbital transfers required to move from the object YOU are orbiting to the object SAN is orbiting?"
    print $ countTransfer m "YOU" "SAN"

-- Answer 1: 119831
-- Answer 2: 322
