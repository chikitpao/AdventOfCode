-- Day05.hs
-- AoC 2019 Day 5: Sunny with a Chance of Asteroids
-- Author: Chi-Kit Pao
--
-- REMARK: Uses own module IntCode


import Data.Maybe ( fromJust )
import IntCode


main :: IO ()
main = do
    putStrLn "Question 1: After providing 1 to the only input instruction and"
    putStrLn " passing all the tests, what diagnostic code does the program"
    putStrLn " produce?"
    program' <- readProgram "Day05_input.txt"
    let result1 = runProgram program' 0 (Just [1]) Nothing in
        case result1 of
            Nothing -> print "ERROR: Abnormal abortion"
            Just endProgram -> print $ fromJust $ stateOutput endProgram
    putStrLn "Question 2: What is the diagnostic code for system ID 5?"
    let result2 = runProgram program' 0 (Just [5]) Nothing in
        case result2 of
            Nothing -> print "ERROR: Abnormal abortion"
            Just endProgram -> print $ fromJust $ stateOutput endProgram

-- Answer1: 13547311
-- Answer2: 236453
