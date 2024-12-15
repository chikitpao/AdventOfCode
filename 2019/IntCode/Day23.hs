-- Day23.hs
-- AoC 2019 Day 23: Category Six
-- Author: Chi-Kit Pao
-- TODO: Finish it
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


data MyComputer = MyComputer {
                     computerNumber:: Int,
                     computerProgramState :: MyProgramState
                     } deriving (Show)

run1 :: [MyProgramState] -> Int -> Int -> [[Int]] -> [[Int]]
run1 computerStates index maxComputers outputList = 
    let newComputerState = fromJust $ runProgramWithState (computerStates !! index) False
        (list1, _:list2) = splitAt index computerStates
        outputList = outputList ++ [fromJust (stateOutput newComputerState)]
        newComputerStates = list1 ++ [newComputerState] ++ list2
        nextIndex = index + 1 in
        if nextIndex >= maxComputers then
            outputList
        else
            run1 newComputerStates nextIndex maxComputers outputList


main :: IO()
main = do
    program <- readProgram "Day23_input.txt"
    let


    let computerStates = [fromJust $ runProgram2 program 0 True (Just [x, -1]) Nothing | x <- [0..49]]
        -- outputList::[[Int]] = []
        -- newOutputList = run1 computerStates 0 50 outputList
        outputs = [stateOutput (computerStates !! x) | x <- [0..49]]
        inputs = [stateInput (computerStates !! x) | x <- [0..49]]
        computerStates1 = [fromJust $ runProgramWithState (computerStates !! x) False | x <- [0..49]]
        outputs1 = [stateOutput (computerStates1 !! x) | x <- [0..49]]
    
    putStrLn "Question 1: What is the Y value of the first packet sent to "
    putStrLn " address 255?"
    print $ show outputs
    print $ show outputs1
    -- putStrLn $ "Answer 1: " ++ show (head result)
    putStr "\n\n"

-- Answer 1: 
-- Answer 2: 
