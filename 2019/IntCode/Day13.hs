-- Day13.hs
-- AoC 2019 Day 13: Care Package
-- Author: Chi-Kit Pao
--
-- REMARK: Uses own module IntCode

import IntCode
import qualified Data.Map as Map
import Data.Maybe (fromJust)


data TileId = Empty | Wall | Block | Paddle | Ball deriving (Enum, Eq, Show)

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
            let result2 = runProgramWithState newProgramState1 True in
            case result2 of 
                Nothing -> Nothing
                Just newProgramState2 -> 
                    let result3 = runProgramWithState newProgramState2 True in
                    case result3 of
                        Nothing -> Nothing
                        Just newProgramState3 ->
                            if stateIsHalted newProgramState3 then
                                Just screen
                            else
                                let (h3, t) = splitAt 3 $ fromJust $ stateOutput newProgramState3
                                    x = head h3
                                    y = h3 !! 1
                                    tileId = h3 !! 2
                                    newScreen = handleTile screen x y tileId
                                    newProgramState4 = createProgramState2 (stateProgram newProgramState3) (stateNextLine newProgramState3) (stateInput newProgramState3) (Just t) (stateRelBase newProgramState3)in
                                part1 newScreen newProgramState4

adjustInput :: Int -> Int -> Int
adjustInput ballX paddleX
    | ballX > paddleX  = 1
    | ballX < paddleX  = -1
    | otherwise = 0

{-
debugCurrentData :: [Int] -> [Char]
debugCurrentData currentData =
    let x = head currentData
        y = currentData !! 1 
        value = currentData !! 2 in
        if x == -1 && y == 0 then
            "Score: " ++ show currentData
        else if fromEnum Ball == value then
            "Ball: " ++ show currentData
        else if fromEnum Paddle == value then
            "Paddle: " ++ show currentData
        else
            "Others: " ++ show currentData
-}

getNewScore:: Int -> Int -> Int -> Maybe Int -> Maybe Int
getNewScore x y value oldScore
    | x == -1 && y == 0 = Just value
    | otherwise = oldScore

getNewBallX:: Int -> Int -> Int -> Int -> Int
getNewBallX x y value oldBallX
    | (x == -1 && y == 0) || fromEnum Ball /= value = oldBallX
    | otherwise = x

getNewPaddleX:: Int -> Int -> Int -> Int -> Int
getNewPaddleX x y value oldPaddleX
    | (x == -1 && y == 0) || fromEnum Paddle /= value = oldPaddleX
    | otherwise = x

part2 :: MyProgramState -> Int -> Int -> Maybe Int-> Maybe Int
part2 programState ballX paddleX score = 
    let result = runProgramWithState programState True in
    case result of
        Nothing -> Nothing
        Just newProgramState -> 
            if stateIsHalted newProgramState then
                score
            else 
                if stateIsHaltedForInput newProgramState then
                    let newProgramState2 = createProgramState (stateProgram newProgramState) (stateNextLine newProgramState) (Just [adjustInput ballX paddleX]) (stateOutput newProgramState) (stateRelBase newProgramState) (stateIsHaltedForInput newProgramState) (stateIsHalted newProgramState) in
                    part2 newProgramState2 ballX paddleX score 
                else
                    let newOutput = fromJust $ stateOutput newProgramState in
                    if length newOutput < 3 then
                        part2 newProgramState ballX paddleX score
                    else
                        let (currentData, newOutput2) = splitAt 3 newOutput
                            x = head currentData
                            y = currentData !! 1 
                            value = currentData !! 2 
                            newScore = getNewScore x y value score
                            newBallX = getNewBallX x y value ballX
                            newPaddleX = getNewPaddleX x y value paddleX
                            newProgramState2 = createProgramState2 (stateProgram newProgramState) (stateNextLine newProgramState) (stateInput newProgramState) (Just newOutput2) (stateRelBase newProgramState) in
                            part2 newProgramState2 newBallX newPaddleX newScore


main :: IO ()
main = do
    program <- readProgram "Day13_input.txt"

    putStrLn "Question 1: How many block tiles are on the screen when the game "
    putStrLn " exits?"
    let screen1 = fromJust $ part1 Map.empty (createProgramState2 program 0 (Just [0]) Nothing 0)
        elems = Map.elems screen1
    putStr "Answer1: "
    print $ (length . filter (== Block)) elems

    putStrLn "Question 2: What is your score after the last block is broken?"
    let assocs = Map.assocs screen1
        balls = filter (\(_, s) -> s == Ball) assocs
        ballX = fst $ fst (head balls)
        paddles = filter (\(_, s) -> s == Paddle) assocs
        paddleX = fst $ fst (head paddles)
        modifiedProgram = setValue 0 2 program
    putStr "Balls: "
    print balls
    putStr "Paddles: "
    print paddles
    print $ fromJust $ part2 (createProgramState2 modifiedProgram 0 (Just []) Nothing 0) ballX paddleX Nothing

-- Answer1: 306
-- Answer2: 15328
