-- Day09.hs
-- AoC 2019 Day 9: Sensor Boost
-- Author: Chi-Kit Pao
--
-- REMARK: Uses own module IntCode

import Control.Exception (assert)
import Data.Function (on)
import Data.Maybe (fromJust)
import IntCode
import Debug.Trace

-- debug = flip trace


main :: IO ()
main = do
    program <- readProgram "Day09_input.txt"
    
    -- "maxBound: 9223372036854775807"
    -- "34463338 * 34463338 = 1187721666102244"
    --print $ "maxBound: " ++ show (maxBound :: Int)
    --print $ "34463338 * 34463338 = " ++ show (34463338 * 34463338)

    putStrLn "Question 1: Once your Intcode computer is fully functional, the"
    putStrLn " BOOST program should report no malfunctioning opcodes when run"
    putStrLn "in test mode; it should only output a single value, the BOOST"
    putStrLn " keycode. What BOOST keycode does it produce?"
    
    let result1 = runProgram program 0 (Just [1]) Nothing in
        case result1 of
            Nothing -> print "ERROR: Abnormal abortion"
            Just endProgram -> print $ last $ fromJust $ stateOutput endProgram

    putStrLn "Question 2: Run the BOOST program in sensor boost mode. What"
    putStrLn" are the coordinates of the distress signal?"
    let result2 = runProgram program 0 (Just [2]) Nothing in
        case result2 of
            Nothing -> print "ERROR: Abnormal abortion"
            Just endProgram -> print $ last $ fromJust $ stateOutput endProgram

-- Answer1: 3780860499
-- Answer2: 33343
