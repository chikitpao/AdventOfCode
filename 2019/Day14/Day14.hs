-- Day14.hs
-- AoC 2019 Day 14: Space Stoichiometry
-- Author: Chi-Kit Pao

import Control.Exception (assert)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (isJust)

data Chemical = Chemical {
                            name :: String
                            , amount :: Int
                         } deriving (Show)

data Reaction = Reaction{
                            output :: Chemical
                            , inputs :: [Chemical]
                        } deriving (Show)

parseChemical :: [Char] -> Chemical
parseChemical s = 
    let amountNameStr = splitOn " " s
        amount = read (head amountNameStr)::Int in
    Chemical (last amountNameStr) amount

parseLine :: [Char] -> Reaction
parseLine line =
    let lhsRhs = splitOn " => " line
        inputStrs = splitOn ", " $ head lhsRhs
        output = parseChemical $ last lhsRhs
        inputs = fmap parseChemical inputStrs in
    Reaction output inputs

addToReactionMap :: Map.Map String Reaction -> [Reaction] -> Map.Map String Reaction
addToReactionMap m [] = m
addToReactionMap m (x:xs) = foldl (\ m2 x2 -> Map.insert (name $ output x2) x2 m2) (Map.insert (name $ output x) x m) xs

oreForFuel :: Map.Map String Reaction -> Map.Map String Chemical -> (Int, Map.Map String Chemical)
oreForFuel reactionMap calculationMap = 
    let required = [e | e <- Map.elems calculationMap, name e /= "ORE" && amount e > 0] in
    if null required
        then (amount $ calculationMap Map.! "ORE", calculationMap)
    else 
        let key = name $ head required
            amount_ = assert(isJust $ Map.lookup  key calculationMap) amount $ calculationMap Map.! key
            reaction = assert(isJust $ Map.lookup key reactionMap) reactionMap Map.! key
            reactionRequired = (ceiling :: Double -> Int)(fromIntegral amount_ / fromIntegral (amount (output reaction))) 
            surplus = reactionRequired * amount (output reaction) - amount_
            l = [(name c, amount (Map.findWithDefault (Chemical (name c) 0) (name c) calculationMap) +  amount c * reactionRequired) | c <- inputs reaction]
            cm1 = if surplus == 0 then Map.delete key calculationMap else Map.insert key (Chemical key (-surplus)) calculationMap
            cm2 = foldl (\m x -> Map.insert (fst x) (uncurry Chemical x) m) cm1 l in
        oreForFuel reactionMap cm2

binarySearch :: Map.Map String Reaction -> Int -> Int -> Int -> Int
binarySearch reactionMap target lowerBound upperBound =
    let average = (upperBound + lowerBound) `div` 2
        ore = assert(upperBound >= lowerBound) fst $ oreForFuel reactionMap (Map.singleton "FUEL" (Chemical "FUEL" average)) in
    if ore == target
        then average
    else if ore < target
        then if lowerBound == upperBound
                then lowerBound
             else binarySearch reactionMap target (average + 1) upperBound
    else
        if lowerBound == upperBound
            then lowerBound - 1
        else binarySearch reactionMap target lowerBound (average - 1)

main :: IO ()
main = do
    -- Example input:
    -- 6 WBVJ, 16 CDVNK => 2 PJBZT
    inputLines <- lines <$> readFile "input.txt"
    let rl = fmap parseLine inputLines
        reactionMap = addToReactionMap Map.empty rl
        calculationMap = Map.singleton "FUEL" (Chemical "FUEL" 1)
        result = oreForFuel reactionMap calculationMap
        answer1 = fst result
    
    putStrLn "Question 1: What is the minimum amount of ORE required to produce exactly 1 FUEL?"
    print answer1

    putStrLn "Question 2: Given 1 trillion ORE, what is the maximum amount of FUEL you can produce?"
    let trillion = 1000000000000::Int
        q = trillion `div` answer1
        ore_2q = assert(q < trillion) fst $ oreForFuel reactionMap $ Map.singleton "FUEL" (Chemical "FUEL" (2*q))
        answer2 = assert(ore_2q > trillion) binarySearch reactionMap trillion q ore_2q
    print answer2

-- Answer 1: 248794
-- Answer 2: 4906796
