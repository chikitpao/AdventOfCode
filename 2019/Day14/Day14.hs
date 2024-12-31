-- Day14.hs
-- AoC 2019 Day 14: Space Stoichiometry
-- Author: Chi-Kit Pao

import Control.Exception (assert)
import Data.List.Split (split, splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (isJust)
import Data.Binary.Get (isEmpty)

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
addToReactionMap m (x:xs) = foldl (\ m2 x2 -> Map.insert (name $ output x2) x2 m2) (f x m) xs
    where f x = Map.insert (name $ output x) x

part1 :: Map.Map String Reaction -> Map.Map String Chemical -> Int
part1 reactionMap calculationMap = 
    let required = [e | e <- Map.elems calculationMap, name e /= "ORE" && amount e > 0] in
    if null required
        then amount $ calculationMap Map.! "ORE"
    else 
        let key = name $ head required
            amount_ = assert(isJust $ Map.lookup  key calculationMap) amount $ calculationMap Map.! key
            reaction = assert(isJust $ Map.lookup key reactionMap) reactionMap Map.! key
            reactionRequired = ceiling( fromIntegral amount_ / fromIntegral (amount (output reaction)))
            surplus = reactionRequired * amount (output reaction) - amount_
            l = [(name c, amount (Map.findWithDefault (Chemical (name c) 0) (name c) calculationMap) +  amount c * reactionRequired) | c <- inputs reaction]
            cm1 = if surplus == 0 then Map.delete key calculationMap else Map.insert key (Chemical key (-surplus)) calculationMap
            cm2 = foldl (\m x -> Map.insert (fst x) (uncurry Chemical x) m) cm1 l in
        part1 reactionMap cm2

main :: IO ()
main = do
    -- Example input:
    -- 6 WBVJ, 16 CDVNK => 2 PJBZT
    inputLines <- lines <$> readFile "input.txt"
    let rl = fmap parseLine inputLines
        reactionMap = addToReactionMap Map.empty rl
        calculationMap = Map.singleton "FUEL" (Chemical "FUEL" 1)
    
    putStrLn "Question 1: What is the minimum amount of ORE required to produce exactly 1 FUEL?"
    print $ part1 reactionMap calculationMap

-- Answer 1: 248794

