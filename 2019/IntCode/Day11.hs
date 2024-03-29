-- Day11.hs
-- AoC 2019 Day 11: Space Police
-- Author: Chi-Kit Pao
--
-- REMARK: Uses own module IntCode

import Control.Exception (assert)
import Data.Maybe (fromJust)
import IntCode
import qualified Data.Map as Map
import Debug.Trace

debug = flip trace

data Dir = North | East | South | West deriving (Enum)

turnPos :: Int -> Int -> Dir -> Int -> (Int, Int, Dir)
turnPos posX posY dir turn = assert(turn == 0 || turn == 1) $
    if turn == 0 then
        -- turn left
        case dir of
                North -> (posX - 1, posY, West)
                East -> (posX, posY - 1, North)
                South -> (posX + 1, posY, East)
                West -> (posX, posY + 1, South)
    else
        -- turn right
        case dir of
            North -> (posX + 1, posY, East)
            East -> (posX, posY + 1, South)
            South -> (posX - 1, posY, West)
            West -> (posX, posY - 1, North)

doPaint :: Map.Map (Int, Int) Int -> MyProgramState -> Int -> Int -> Int -> Dir -> Int -> Maybe (Map.Map (Int, Int) Int)
doPaint panels programState step posX posY dir oldColor = assert(null (fromJust (stateInput programState))) $
    let program = stateProgram programState
        nextLine = stateNextLine programState
        input = ((++) <$> stateInput programState  <*> Just [oldColor])
        output = stateOutput programState
        relBase = stateRelBase programState
        result1 = runProgram3 program nextLine True relBase input output in
        case result1 of
            Nothing -> Nothing
            Just newProgramState1 -> 
                let newOutput1 = stateOutput newProgramState1
                    valuefunc _ = newOutput1
                    result2 = runProgram3 (stateProgram newProgramState1) (stateNextLine newProgramState1) True (stateRelBase newProgramState1) (stateInput newProgramState1) newOutput1 in
                    case result2 of
                        Nothing -> Nothing
                        Just newProgramState2 -> 
                            if stateIsHalted newProgramState2 then
                                Just panels
                            else
                                let newPanels = Map.alter valuefunc (posX, posY) panels
                                    newOutput2 = stateOutput newProgramState2
                                    (newPosX, newPosY, newDir) = turnPos posX posY dir (fromJust newOutput2) 
                                    newColor = Map.findWithDefault 0 (newPosX, newPosY) newPanels 
                                    programStateForIntput = createProgramState (stateProgram newProgramState2) (stateNextLine newProgramState2) (stateInput newProgramState2) newOutput2 (stateRelBase newProgramState2) in
                                    doPaint newPanels programStateForIntput (step + 1) newPosX newPosY newDir newColor -- `debug` ("steps " ++ show step ++ " pos " ++ show newPosX ++ " " ++ show newPosY ++ " dir " ++ show (fromEnum newDir))



getRowPattern :: Map.Map (Int, Int) Int -> [Int] -> Int -> [Char]
getRowPattern drawing xvalues y = [if Map.findWithDefault 0 (x, y) drawing == 1 then '#' else ' ' | x <- xvalues]

paintDrawing :: Map.Map (Int, Int) Int -> IO()
paintDrawing drawing = do
    let keys = Map.keys drawing
        (xvalues, yvalues) = unzip keys
        minX = minimum xvalues
        maxX = maximum xvalues
        minY = minimum yvalues
        maxY = maximum yvalues
        rows = [getRowPattern drawing xvalues y | y <- [minY..maxY]]
    mapM_ print rows

main :: IO ()
main = do
    program <- readProgram "Day11_input.txt"

    putStrLn "Question 1: Build a new emergency hull painting robot and run"
    putStrLn " the Intcode program on it. How many panels does it paint at"
    putStrLn " least once?"
    let drawing1 = fromJust $ doPaint Map.empty (createProgramState program 0 (Just []) Nothing 0) 1 0 0 North 0 in
        print $ Map.size drawing1

    putStrLn "Question 2: After starting the robot on a single white panel "
    putStrLn " instead, what registration identifier does it paint on your "
    putStrLn " hull?"
    let drawing2 = fromJust $ doPaint Map.empty (createProgramState program 0 (Just []) Nothing 0) 1 0 0 North 1 in
        paintDrawing drawing2

-- Answer1: 2172
-- Answer2: JELEFGHP
