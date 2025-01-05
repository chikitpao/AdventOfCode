-- Day22.hs
-- AoC 2019 Day 22: Slam Shuffle
-- Author: Chi-Kit Pao

import Data.List (elemIndex, isPrefixOf, sortBy)
import Data.Maybe (fromJust)

cut :: Int -> [Int] -> [Int]
cut number cards
    | number > 0 = let (a, b) = splitAt number cards in
                   b ++ a
    | number < 0 = let (a, b) = splitAt (length cards + number) cards in
                   b ++ a
    | otherwise = cards

dealWithIncrement :: Int -> [Int] -> [Int]
dealWithIncrement number cards = 
    let permutationList = sortBy (\(_,a) (_,b) -> compare a b)  [ (i, (i * number) `mod` length cards) | i <- [0..length cards-1]] in
    [cards !! a | (a, _) <- permutationList]

executeInstruction :: [Int] -> [Char] -> [Int]
executeInstruction cards instruction
        | "deal into new stack" `isPrefixOf` instruction = reverse cards
        | "cut "  `isPrefixOf` instruction = cut (read (drop (length "cut ") instruction)::Int) cards
        | "deal with increment " `isPrefixOf` instruction = dealWithIncrement (read (drop (length "deal with increment ") instruction)::Int) cards
        | otherwise = error ("executeInstruction: Unknown instruction " ++ instruction)

part1 :: [String] -> [Int] -> [Int]
part1 instructions cards = foldl executeInstruction cards instructions

main :: IO ()
main = do
    inputLines <- lines <$> readFile "input.txt"
   
    putStrLn "Question 1: What is the position of card 2019?"
    let oldCards = [0..10006]
        newCards = part1 inputLines oldCards
    print $ fromJust $ elemIndex 2019 newCards

-- Answer 1: 2496
