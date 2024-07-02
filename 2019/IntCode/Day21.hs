-- Day21.hs
-- AoC 2019 Day 21: Springdroid Adventure
-- Author: Chi-Kit Pao
--
-- REMARK: Uses own module IntCode

import Control.Exception (assert)
import Control.Monad
import IntCode
import Data.Char (chr, ord)
import Data.List (elemIndex, find)
import Data.Maybe (fromJust, isNothing, isJust)
import Debug.Trace
import GHC.Exts.Heap (StgInfoTable(code))

debug = flip trace

runSpringscript :: MyProgram -> [Int] -> ([Int], [Int])
runSpringscript program script = (msg, result)
    where currentState = fromJust $ runProgram2 program 0 False (Just script) Nothing
          output = fromJust $ stateOutput currentState
          msg = takeWhile (<= 255) output
          result = dropWhile (<= 255) output

main :: IO()
main = do
    program <- readProgram "Day21_input.txt"
    -- let input1 = Just $ map ord $ "NOT D J\n" ++ "WALK\n"
    -- let input1 = Just $ map ord $ "NOT A J\n" ++ "WALK\n"
    let input1 = map ord $ "NOT A J\n"
                        ++ "NOT B T\n"
                        ++ "OR T J\n"
                        ++ "NOT C T\n"
                        ++ "OR T J\n"
                        ++ "AND D J\n"
                        ++ "WALK\n"
        (msg, result) = runSpringscript program input1
    putStrLn $ map chr msg
    
    putStrLn "Question 1: What amount of hull damage does it report?"
    if null result
        then putStrLn "Try Again for Answer 1!"
        else
            putStrLn $ "Answer 1: " ++ show (head result)
    putStr "\n\n"


    let input2 = map ord $ "NOT A J\n"
                        ++ "NOT B T\n"
                        ++ "OR T J\n"
                        ++ "NOT C T\n"
                        ++ "OR T J\n"
                        ++ "AND D J\n"
                        ++ "NOT E T\n"
                        ++ "NOT T T\n"
                        ++ "OR H T\n"
                        ++ "AND T J\n"
                        ++ "RUN\n"
        (msg, result) = runSpringscript program input2
    putStrLn $ map chr msg

    putStrLn "Question 2: What amount of hull damage does the springdroid now report?"
    if null result
        then putStrLn "Try Again for Answer 2!"
        else
            putStrLn $ "Answer 2: " ++ show (head result)

-- Answer 1: 19361332
-- Answer 2: 1143351187
