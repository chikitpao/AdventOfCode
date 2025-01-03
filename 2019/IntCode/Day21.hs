-- Day21.hs
-- AoC 2019 Day 21: Springdroid Adventure
-- Author: Chi-Kit Pao
--
-- REMARK: Uses own module IntCode

import IntCode
import Data.Char (chr, ord)
import Data.Maybe (fromJust)

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
        (msg1, result1) = runSpringscript program input1
    putStrLn $ map chr msg1
    
    putStrLn "Question 1: What amount of hull damage does it report?"
    if null result1
        then putStrLn "Try Again for Answer 1!"
        else
            putStrLn $ "Answer 1: " ++ show (head result1)
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
        (msg2, result2) = runSpringscript program input2
    putStrLn $ map chr msg2

    putStrLn "Question 2: What amount of hull damage does the springdroid now report?"
    if null result2
        then putStrLn "Try Again for Answer 2!"
        else
            putStrLn $ "Answer 2: " ++ show (head result2)

-- Answer 1: 19361332
-- Answer 2: 1143351187
