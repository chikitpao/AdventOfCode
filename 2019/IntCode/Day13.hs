-- Day13.hs
-- AoC 2019 Day 13: Care Package
-- Author: Chi-Kit Pao
--
-- REMARK: Uses own module IntCode

import Control.Exception (assert)
import IntCode
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Debug.Trace

debug = flip trace

data TileId = Empty | Wall | Block | Paddle | Ball deriving (Enum, Eq)

handleTile :: Map.Map (Int, Int) TileId -> Int -> Int -> Int -> Map.Map (Int, Int) TileId
handleTile oldScreen x y tildId_ =
    let pos = (x, y)
        tileId = toEnum tildId_::TileId
        valuefunc _ = Just tileId
        existing = Map.findWithDefault Empty pos oldScreen in
    if tileId == Empty then
        oldScreen
    else if existing == Empty then
        Map.insert pos tileId oldScreen
    else if existing == Block && tileId == Ball then 
        Map.alter valuefunc pos oldScreen
    else
        oldScreen


part1 :: Map.Map (Int, Int) TileId -> MyProgramState -> Maybe (Map.Map (Int, Int) TileId)
part1 screen programState = 
    let result1 = runProgramWithState programState True in
    case result1 of
        Nothing -> Nothing
        Just newProgramState1 -> 
            let x = fromJust $ stateOutput newProgramState1
                result2 = runProgramWithState newProgramState1 True in
            case result2 of 
                Nothing -> Nothing
                Just newProgramState2 -> 
                    let y = fromJust $ stateOutput newProgramState2 
                        result3 = runProgramWithState newProgramState2 True in
                    case result3 of 
                        Nothing -> Nothing
                        Just newProgramState3 -> 
                            let tileId = fromJust $ stateOutput newProgramState3
                                newScreen = handleTile screen x y tileId in
                            if stateIsHalted newProgramState3 then
                                Just newScreen
                            else
                                part1 newScreen newProgramState3
                            

main :: IO ()
main = do
    program <- readProgram "Day13_input.txt"

    putStrLn "Question 1:How many block tiles are on the screen when the game "
    putStrLn " exits?"
    let screen1 = fromJust $ part1 Map.empty (createProgramState program 0 (Just []) Nothing 0)
        elems = Map.elems screen1
    print $ (length . filter (== Block)) elems
    

-- Answer1: 306
-- Answer2: 
