-- Day15.hs
-- AoC 2019 Day 15: Oxygen System
-- Author: Chi-Kit Pao
--
-- REMARK: Uses own module IntCode

import Control.Exception (assert)
import IntCode
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Debug.Trace

debug = flip trace

data Command = Reserved | North | South | West | East deriving (Enum, Eq, Show)
data Status = Wall | Empty | O2System deriving (Enum, Eq, Show)
data Maze = Maze {  visiting :: Map.Map (Int, Int) Int -- pos -> status
                    , visited :: Map.Map (Int, Int) Int -- pos -> status
                 }deriving (Show)


nextPos :: (Int, Int) -> Command -> (Int, Int)
nextPos pos command = 
    case command of
        North -> (fst pos, snd pos - 1)
        South -> (fst pos, snd pos + 1)
        West -> (fst pos - 1, snd pos)
        East -> (fst pos + 1, snd pos)

nextProgramState:: Map.Map (Int, Int) MyProgramState -> (Int, Int) -> Int -> MyProgramState
nextProgramState programStates currentPos commandId = 
    let programState = programStates Map.! currentPos in
    createProgramState2 (stateProgram programState) (stateNextLine programState) (Just [commandId]) Nothing (stateRelBase programState)

isNewPos :: Maze -> (Int, Int) -> Bool
isNewPos maze pos = Map.notMember pos (visiting maze) && Map.notMember pos (visited maze)

updateMaze :: Maze -> [((Int, Int), Int)] -> [((Int, Int), Int)] -> Maze
updateMaze maze vtingEmpty vtingWall = Maze (Map.fromList vtingEmpty) (Map.union (Map.union (visited maze) (visiting maze)) (Map.fromList vtingWall))

-- Breadth-first search
part1 :: Map.Map (Int, Int) MyProgramState -> Maze -> Int -> Int
part1 programStates maze steps =
    -- nextSteps1 -> Map with key = next positions and values = [current positions, state after next program call]
    let nextStepsTemp = [(nextPos vPos (toEnum i::Command), (vPos, fromJust $ runProgramWithState (nextProgramState programStates vPos i) True))
             | vPos <- Map.keys $ visiting maze, i <- [1..4]] 
        nextStepsFiltered = [ns | ns <- nextStepsTemp, Map.notMember (fst ns) (visiting maze) && Map.notMember (fst ns) (visited maze)]in
    -- if any (\x -> (head $ fromJust (stateOutput (snd (snd x))) == (fromEnum O2System))) nextStepsFiltered  then
    if any (\x -> head (fromJust $ stateOutput (snd (snd x))) == fromEnum O2System) nextStepsFiltered then
        steps + 1
    else
        let nextSteps = Map.fromList nextStepsFiltered-- Remove duplicated next positions
            vting = [(f, head $ fromJust $ stateOutput s2) | (f, (_, s2)) <- Map.assocs nextSteps]
            vtingEmpty = [(f, s) | (f, s) <- vting, s == fromEnum Empty]
            vtingWall = [(f, s) | (f, s) <- vting, s == fromEnum Wall]
            newMaze = updateMaze maze vtingEmpty vtingWall
            (posEmpty, _) = unzip vtingEmpty
            newProgramStates = Map.fromList [(f, createProgramState2 (stateProgram s2) (stateNextLine s2) (Just []) (Just []) (stateRelBase s2)) | (f, (_, s2)) <- Map.assocs nextSteps, f `elem` posEmpty] in
        part1 newProgramStates newMaze (steps + 1)


main :: IO ()
main = do
    program <- readProgram "Day15_input.txt"

    putStrLn "Question 1: What is the fewest number of movement commands"
    putStrLn " required to move the repair droid from its starting position"
    putStrLn " to the location of the oxygen system?"
    let programState = createProgramState2 program 0 (Just []) Nothing 0
    print $ part1 (Map.singleton (0, 0) programState) (Maze (Map.singleton (0, 0) (fromEnum Empty)) Map.empty) 0

    putStrLn "Question 2: Use the repair droid to get a complete map of the"
    putStrLn " area. How many minutes will it take to fill with oxygen?"

-- Answer1: 270
-- Answer2: 
