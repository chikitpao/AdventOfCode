-- Day22.hs
-- AoC 2019 Day 22: Slam Shuffle
-- Author: Chi-Kit Pao
-- Command for build: ghc -package modular-arithmetic -XDataKinds -XTypeApplications --make -Wall Day22.hs

import Data.Bifunctor (bimap)
import Data.Bits
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
-- cabal install modular-arithmetic
import Data.Modular

mergeVariables :: Int -> (Int, Int) -> [Char] -> (Int, Int)
mergeVariables cardCount (a, b) instruction
        | "deal into new stack" `isPrefixOf` instruction = ((-a) `mod` cardCount, (-b - 1) `mod` cardCount)
        | "cut "  `isPrefixOf` instruction = (a, (b - (read (drop (length "cut ") instruction)::Int)) `mod` cardCount)
        | "deal with increment " `isPrefixOf` instruction = let factor = read (drop (length "deal with increment ") instruction)::Int in
                                                            ((a * factor) `mod` cardCount, (b * factor) `mod` cardCount)
        | otherwise = error ("mergeVariables: Unknown instruction " ++ instruction)

calculateVariables :: [String] -> (Int, Int) -> Int -> (Int, Int)
calculateVariables instructions vars cardCount =
    let f = mergeVariables cardCount in
    foldl f vars instructions

multVars :: (Integer, Integer) -> (Integer, Integer) -> Integer -> (Integer, Integer)
multVars (a, b) (c, d) modValue = ((a * c) `mod` modValue, (a * d + b) `mod` modValue)

fastExpontiation :: (Integer, Integer) -> Integer -> Integer -> (Integer, Integer) -> (Integer, Integer)
fastExpontiation sqVars modValue remaining resultVars =
    let lsb = remaining .&. 1
        resultVars' = if lsb == 0 then resultVars else multVars sqVars resultVars modValue in
    if remaining == 1 then resultVars' else fastExpontiation (multVars sqVars sqVars modValue) modValue (remaining `div` 2) resultVars'

main :: IO ()
main = do
    inputLines <- lines <$> readFile "input.txt"
   
    putStrLn "Question 1: What is the position of card 2019?"
    let oldCardCount = 10007
        oldVars = calculateVariables inputLines (1, 0) oldCardCount
    print $ (fst oldVars * 2019 + snd oldVars) `mod` oldCardCount

    putStrLn "Question 2: What number is on the card that ends up in position 2020?"
    -- Shuffle is a permutation and will change the position of card position x : x -> f(x) = a * x + b, initially, a = 1 and b = 0
    let newCardCount = 119315717514047
        repetition = 101741582076661
        newVars = calculateVariables inputLines (1, 0) newCardCount
        -- fast exponentiation by squaring
        newVars' = fastExpontiation  (bimap toInteger toInteger newVars) (toInteger newCardCount) repetition (1, 0)
        rhs = (2020 - snd newVars') `mod` toInteger newCardCount
        invValue = unMod $ fromJust $ inv (toMod @119315717514047 (toInteger $ fst newVars')) -- hard-wired card count
        answer2 = (toInteger rhs * invValue) `mod` toInteger newCardCount
    print answer2

-- Answer 1: 2496
-- Answer 2: 56894170832118
