-- Day25.hs
-- AoC 2019 Day 25: Cryostasis
-- Author: Chi-Kit Pao
--
-- REMARK: Uses own module IntCode

import Control.Monad (unless)
import IntCode
import Data.Char (chr, ord)
import Data.Maybe (fromJust)

-- Possible instructions are:
-- - Movement via north, south, east, or west.
-- - To take an item the droid sees in the environment, use the command take 
--   <name of item>. For example, if the droid reports seeing a red ball, you
--   can pick it up with take red ball.
-- - To drop an item the droid is carrying, use the command drop <name of 
--   item>. For example, if the droid is carrying a green ball, you can drop 
--   it with drop green ball.
-- - To get a list of all of the items the droid is currently carrying, use the
--   command inv (for "inventory").

doLoop :: MyProgramState -> IO ()
doLoop currentState = do
    putStrLn $ map chr $ fromJust $ stateOutput currentState
    tempCommand <- getLine
    let modifiedState = createProgramState (stateProgram currentState)
                     (stateNextLine currentState)
                     (Just $ map ord (tempCommand ++ "\n"))
                     (stateOutput currentState)
                     (stateRelBase currentState)
                     (stateIsHaltedForInput currentState)
                     (stateIsHalted currentState)
    unless (stateIsHalted currentState) (doLoop (fromJust $ runProgramWithState modifiedState False))


main :: IO()
main = do
    program <- readProgram "Day25_input.txt"
    let inv = Just $ map ord "inv\n"
        currentState = fromJust $ runProgram2 program 0 False inv Nothing
    doLoop currentState -- start the first iteration 

-- After trail and error, I found out that with the items space heater, 
-- antenna, spool of cat6 and klein bottle, the droid has the correct weight 
-- to pass the check at the Pressure-Sensitive Floor.
--
-- A loud, robotic voice says "Analysis complete! You may proceed." and you enter the cockpit.
-- Santa notices your small droid, looks puzzled for a moment, realizes what has happened, and radios your ship directly.
-- "Oh, hello! You should be able to get in by typing 8462464 on the keypad at the main airlock."

-- Answer: 8462464
