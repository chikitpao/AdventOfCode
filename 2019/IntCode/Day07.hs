-- Day07.hs
-- AoC 2019 Day 7: Amplification Circuit
-- Author: Chi-Kit Pao
--
-- REMARK: Uses own module IntCode

import Data.Function (on)
import Data.List (maximumBy, permutations)
import Data.Maybe ( fromJust )
import IntCode



pairToList :: (a, a) -> [a]
pairToList (x,y) = [x,y]

thruster :: [Int] -> [Int] -> Int -> Int -> Int -> ([Int], Int)
thruster program' input input2 current end = let result = runProgram program' 0 (Just [input !! current, input2]) Nothing in
                                                 case result of
                                                    Nothing -> (input, -1)
                                                    Just endProgram -> if current + 1 == end then
                                                                            (input, fromJust $ get4th endProgram)
                                                                        else
                                                                            thruster program' input (fromJust $ get4th endProgram) (current + 1) end

thrusterChain :: [Int] -> [Int]-> ([Int], Int)
thrusterChain program' input = thruster program' input 0 0 5

part1 :: [Int] -> ([Int], Int)
part1 program' = maximumBy (compare `on` snd) [thrusterChain program' p  | p <- permutations [0..4]]

main :: IO ()
main = do
    putStrLn "Question 1: Try every combination of phase settings on the"
    putStrLn " amplifiers. What is the highest signal that can be sent to the"
    putStrLn" thrusters?"
    program' <- program "Day07_input.txt"
    print $ part1 program' -- Output: ([0,3,2,4,1],38500)


-- Answer1: 38500
-- Answer2: 
