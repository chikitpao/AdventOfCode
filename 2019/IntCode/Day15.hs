-- Day15.hs
-- AoC 2019 Day 15: Oxygen System
-- Author: Chi-Kit Pao
--
-- REMARK: Uses own module IntCode

import IntCode
    ( createProgramState2,
      readProgram,
      runProgramWithState,
      MyProgramState(stateRelBase, stateOutput, stateProgram,
                     stateNextLine) )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (fromJust)


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
        _ -> error "nextPos: Invalid command"

nextProgramState:: Map.Map (Int, Int) MyProgramState -> (Int, Int) -> Int -> MyProgramState
nextProgramState programStates currentPos commandId = 
    let programState = programStates Map.! currentPos in
    createProgramState2 (stateProgram programState) (stateNextLine programState) (Just [commandId]) Nothing (stateRelBase programState)

updateMaze :: Maze -> [((Int, Int), Int)] -> [((Int, Int), Int)] -> Maze
updateMaze maze vtingEmpty vtingWall = Maze (Map.fromList vtingEmpty) (Map.union (Map.union (visited maze) (visiting maze)) (Map.fromList vtingWall))

-- Breadth-first search
part1 :: Map.Map (Int, Int) MyProgramState -> Maze -> Int -> Int
part1 programStates maze steps =
    -- nextStepsTemp -> Map with key = next position and value = [current position, state after next program call]
    let nextStepsTemp = [(nextPos vPos (toEnum i::Command), (vPos, fromJust $ runProgramWithState (nextProgramState programStates vPos i) True))
             | vPos <- Map.keys $ visiting maze, i <- [1..4]] 
        nextStepsFiltered = [ns | ns <- nextStepsTemp, Map.notMember (fst ns) (visiting maze) && Map.notMember (fst ns) (visited maze)]in
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

updateMaze2 :: Maze -> [((Int, Int), Int)] -> [((Int, Int), Int)] -> [((Int, Int), Int)] -> Maze
updateMaze2 maze vtingEmpty vtingWall vtingO2System = 
    let vting = Map.union (Map.fromList vtingEmpty) (Map.fromList vtingO2System)
        vted = Map.union (Map.union (visited maze) (visiting maze)) (Map.fromList vtingWall) in
            Maze vting vted

-- Breadth-first search like part1, but finish when the whole map is explored.
-- Returns the whole map.
part2Maze :: Map.Map (Int, Int) MyProgramState -> Maze -> Maze
part2Maze programStates maze =
    -- nextStepsTemp -> Map with key = next position and value = [current position, state after next program call]
    let nextStepsTemp = [(nextPos vPos (toEnum i::Command), (vPos, fromJust $ runProgramWithState (nextProgramState programStates vPos i) True))
             | vPos <- Map.keys $ visiting maze, i <- [1..4]] 
        nextStepsFiltered = [ns | ns <- nextStepsTemp, Map.notMember (fst ns) (visiting maze) && Map.notMember (fst ns) (visited maze)]in
        let nextSteps = Map.fromList nextStepsFiltered-- Remove duplicated next positions
            vting = [(f, head $ fromJust $ stateOutput s2) | (f, (_, s2)) <- Map.assocs nextSteps] in
        if null vting then
            maze
        else
            let vtingEmpty = [(f, s) | (f, s) <- vting, s == fromEnum Empty]
                vtingWall = [(f, s) | (f, s) <- vting, s == fromEnum Wall]
                vtingO2System = [(f, s) | (f, s) <- vting, s == fromEnum O2System]
                newMaze = updateMaze2 maze vtingEmpty vtingWall vtingO2System
                (posEmpty, _) = unzip vtingEmpty
                (posO2System, _) = unzip vtingO2System
                newProgramStates = Map.fromList [(f, createProgramState2 (stateProgram s2) (stateNextLine s2) (Just []) (Just []) (stateRelBase s2)) 
                    | (f, (_, s2)) <- Map.assocs nextSteps, f `elem` posEmpty || f `elem` posO2System] in
            part2Maze newProgramStates newMaze

fillOxygen :: [((Int, Int), Int)] -> [((Int, Int), Int)] -> [((Int, Int), Int)] -> Int -> Int
fillOxygen vting vted notVisited steps = 
    let (posNotVisited, _ ) = unzip notVisited
        nextStepsTemp = [nextPos f (toEnum i::Command) | (f, _) <- vting, i <- [1..4]]
        nextStepsUnique = Set.toList (Set.fromList nextStepsTemp)
        nextStepsFiltered = [ns | ns <- nextStepsUnique, ns `elem` posNotVisited]
        newVisiting = [nv | nv <- notVisited, fst nv `elem` nextStepsFiltered]
        newNotVisited = [nv | nv <- notVisited,  fst nv `notElem` nextStepsFiltered] 
        newVisited = vted ++ vting in
        if null newVisiting then
            steps
        else
            fillOxygen newVisiting newVisited newNotVisited (steps + 1)


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
    let maze = part2Maze (Map.singleton (0, 0) programState) (Maze (Map.singleton (0, 0) (fromEnum Empty)) Map.empty)
    -- print $ show (Map.size (visiting maze)) ++ " " ++ show (Map.size (visited maze)) -- "0 1659"
        newVisiting = [t | t <- Map.assocs (visited maze), snd t == fromEnum Empty]
        newVisited = [t | t <- Map.assocs (visited maze), snd t == fromEnum Wall]
        o2Pos = [t | t <- Map.assocs (visited maze), snd t == fromEnum O2System]
    print $ fillOxygen o2Pos newVisited newVisiting 0

-- Answer1: 270
-- Answer2: 364

