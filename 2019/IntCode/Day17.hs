-- Day17.hs
-- AoC 2019 Day 17: Set and Forget
-- Author: Chi-Kit Pao
--
-- REMARK: Uses own module IntCode
-- REMARK: Solution for Part 2 was done by hand.

import Control.Exception (assert)
import IntCode
import Data.Char (chr, ord)
import Data.Maybe (fromJust, isNothing)
import Debug.Trace

debug = flip trace

data Turn = LeftTurn | RightTurn deriving (Enum, Eq)
data Direction = North | East | South | West deriving (Enum, Eq)

convertToMap :: [Int] -> [[Char]]
convertToMap = filter (not . null) . lines . map chr

getSurrounding :: [[Char]] -> Int -> Int -> [Char]
getSurrounding input x y = [(input !! y) !! x, (input !! (y-1)) !! x, (input !! y) !! (x+1), (input !! (y+1)) !! x, (input !! y) !! (x+1)]

checkSurrounding :: [Char] -> Bool
checkSurrounding = all (== '#')

countIntersections :: [[Char]] -> Int
countIntersections input = 
-- Assumes the intesection means 4-way-crossing here ("+" form)
-- In my input, intersections and scaffolds around it are '#'.(*)
    let columnCount = length (head input)
        rowCount = length input in
        sum [if checkSurrounding (getSurrounding input x y) then x * y else 0 | x <- [1..(columnCount - 2)], y <- [1..(rowCount - 2)]]

getRobots :: [[Char]] -> [(Int, Int, Char)]
getRobots input = 
    let columnCount = length (head input)
        rowCount = length input in
    [(x, y, (input !! y) !! x) | x <- [0..(columnCount - 1)], y <- [0..(rowCount - 1)], ((input !! y) !! x) `elem` "^v<>"]


getRobotPos (a, b, _) = (a, b)
getRobotDir (_, _, a) = a

getNextMovement :: [[Char]] -> (Int, Int) -> Int -> Direction -> (Maybe (Int, Int), Maybe(Turn, Direction))
getNextMovement input prevPos pathLength direction = 
    let columnCount = length (head input)
        rowCount = length input
        x = fst prevPos
        y = snd prevPos in
    case direction of
        North -> let newY = y - pathLength 
                     left = if x == 0 then '.' else input !! newY !! (x - 1) 
                     right = if x == columnCount - 1 then '.' else (input !! newY) !! (x + 1) in
                if left == '#' then
                    (Just (x, newY), Just (LeftTurn, West))
                else if right == '#' then
                    (Just (x, newY), Just (RightTurn, East))
                else
                    (Nothing, Nothing)
        East -> let newX = x + pathLength
                    up = if y == 0 then '.' else input !! (y - 1) !! newX
                    down = if y == rowCount - 1 then '.' else input !! (y + 1) !! newX in
                if up == '#' then
                    (Just (newX, y), Just (LeftTurn, North))
                else if down == '#' then
                    (Just (newX, y), Just (RightTurn, South))
                else
                    (Nothing, Nothing)
        South -> let newY = y + pathLength
                     left = if x == 0 then '.' else input !! newY !! (x - 1) 
                     right = if x == columnCount - 1 then '.' else (input !! newY) !! (x + 1) in
                if left == '#' then
                    (Just (x, newY), Just (RightTurn, West))
                else if right == '#' then
                    (Just (x, newY), Just (LeftTurn, East))
                else
                    (Nothing, Nothing)
        West -> let newX = x - pathLength
                    up = if y == 0 then '.' else input !! (y - 1) !! newX
                    down = if y == rowCount - 1 then '.' else input !! (y + 1) !! newX in
                if up == '#' then
                    (Just (newX, y), Just (RightTurn, North))
                else if down == '#' then
                    (Just (newX, y), Just (LeftTurn, South))
                else
                    (Nothing, Nothing)

evaluateScaffolds :: [[Char]] -> (Int, Int) -> [Char] -> (Turn, Direction) -> [Char] -> [Char]
evaluateScaffolds input prevPos sequence prevTurn scaffolds =
    if null scaffolds then
        sequence
    else
        let sep = if null sequence then "" else "," 
            turn = fst prevTurn
            turnChar = if turn == LeftTurn then "L" else "R"
            direction = snd prevTurn
            nextSequence = sequence ++ sep ++ turnChar ++ "," ++ show (length scaffolds)
            (nextPos, nextTurn) = getNextMovement input prevPos (length scaffolds) direction in
        if isNothing nextPos then
            nextSequence
        else
            getMainSequence input (fromJust nextPos) nextSequence (fromJust nextTurn)

getMainSequence :: [[Char]] -> (Int, Int) -> [Char] -> (Turn, Direction) -> [Char]
getMainSequence input prevPos sequence prevTurn = 
    let columnCount = length (head input)
        rowCount = length input
        direction = snd prevTurn
        prevX = fst prevPos
        prevY = snd prevPos in
        if direction == North && prevY > 0 then
            evaluateScaffolds input prevPos sequence prevTurn $ takeWhile (== '#') [input !! y !! prevX | y <- [prevY-1, prevY-2..0], y >= 0]
        else if direction == East && prevX + 1 < columnCount then
           evaluateScaffolds input prevPos sequence prevTurn $ takeWhile (== '#') [input !! prevY !! x | x <- [prevX+1..columnCount-1], x < columnCount]
        else if direction == South && prevY + 1 < rowCount then
            evaluateScaffolds input prevPos sequence prevTurn $ takeWhile (== '#') [input !! y !! prevX | y <- [prevY+1..rowCount-1], y < rowCount]
        else if direction == West && prevX > 0 then
            evaluateScaffolds input prevPos sequence prevTurn $ takeWhile (== '#') [input !! prevY !! x | x <- [prevX-1, prevX-2..0], x >= 0]
        else
            error "getMainSequence got to an dead end!"

getFirstTurn :: [[Char]] -> (Int, Int, Char) -> (Turn, Direction)
getFirstTurn input robot =
    let pos = getRobotPos robot
        x = fst pos
        y = snd pos
        dir = getRobotDir robot
        up = if y == 0 then '.' else (input !! (y - 1)) !! x
        left = if x == 0 then '.' else (input !! y) !! (x - 1) in
    case dir of
        '^' -> if left == '#' then (LeftTurn, West) else (RightTurn, East)
        'v' -> if left == '#' then (RightTurn, West) else (LeftTurn, East)
        '<' -> if up == '#' then (RightTurn, North) else (LeftTurn, South)
        '>' -> if up == '#' then (LeftTurn, North) else (RightTurn, South)
        _ -> error "Invalid robot"

main :: IO ()
main = do
    program <- readProgram "Day17_input.txt"
    print (length $ programCode program)

    let programState = createProgramState2 program 0 Nothing Nothing 0
        programResult = runProgramWithState programState False 
        output = convertToMap $ fromJust $ stateOutput (fromJust programResult)
{-
    mapM_ print output
    putStrLn ""
-}
    putStrLn "Question 1: What is the sum of the alignment parameters for the"
    putStrLn " scaffold intersections?"
    print $ "Answer 1: " ++ show (countIntersections output)

    putStrLn "Question 2: After visiting every part of the scaffold at least "
    putStrLn " once, how much dust does the vacuum robot report it has "
    putStrLn " collected?"
    let modifiedProgram = setValue 0 2 program
        robots = getRobots output
        robot = assert (length robots == 1) head robots
    -- Output: Robot: (42,14,'^')
    print $ "Robot: " ++ show robot    
    --  Output: Complete Sequence: L,6,R,8,R,12,L,6,L,8,L,10,L,8,R,12,L,6,R,8,R,12,L,6,L,8,L,8,L,10,L,6,L,6,L,10,L,8,R,12,L,8,L,10,L,6,L,6,L,10,L,8,R,12,L,6,R,8,R,12,L,6,L,8,L,8,L,10,L,6,L,6,L,10,L,8,R,12"
    let completeSequence = getMainSequence output (getRobotPos robot) "" (getFirstTurn output robot)
    print $ "Complete Sequence: " ++ completeSequence
    -- A (start) might be: "L,6", "L,6,R,8", "L,6,R,8,R,12", "L,6,R,8,R,12,L,6", "L,6,R,8,R,12,L,6,L,8"
    -- B (end) might be: "R,12", "L,8,R,12", "L,10,L,8,R,12", "L,6,L,10,L,8,R,12". Especially start and
    --   end cannot use the same function since there is not common sequence found for A and B.
    -- Found main routine and the three functions by hand. See AoC2019_Day17_Part2.png.
    -- Main routine: A,B,A,C,B,C,B,A,C,B
    -- A: L,6,R,8,R,12,L,6,L,8
    -- B: L,10,L,8,R,12
    -- C: L,8,l,10,L,6,L,6
    let mainRoutine = map ord "A,B,A,C,B,C,B,A,C,B\n"
        funcA = map ord "L,6,R,8,R,12,L,6,L,8\n"
        funcB = map ord "L,10,L,8,R,12\n"
        funcC = map ord "L,8,L,10,L,6,L,6\n"
        prompt = map ord "n\n"
        input = mainRoutine ++ funcA ++ funcB ++ funcC ++ prompt
        programState2 = createProgramState2 modifiedProgram 0 (Just input) Nothing 0
        programState3 = runProgramWithState programState2 False
        output = fromJust $ stateOutput $ fromJust programState3
        -- programState2 = createProgramState2 modifiedProgram 0 (Just []) Nothing 0
        -- programState3 = runPart2 programState2 input
        -- output = fromJust $ stateOutput $ fromJust programState3 
    print $ "Answer 2: " ++ show (last output)
    

-- Answer1: 7780
-- Answer2: 1075882

{-
"........................................#########......"
"........................................#.......#......"
"........................................#.......#......"
"........................................#.......#......"
"........................................#.......#......"
"........................................#.......#......"
"....................................#############......"
"....................................#...#.............."
"....................................#...#.............."
"....................................#...#.............."
"....................................#...#########......"
"....................................#...........#......"
"..............................#########.........#......"
"..............................#.....#.#.........#......"
"............#############.....#.....######^.....#......"
"............#.................#.......#.........#......"
"..#######...#.............###########.#.........#......"
"..#.....#...#.............#...#.....#.#.........#......"
"#######.#...#.............#...#.....#.#.........#......"
"#.#...#.#...#.............#...#.....#.#.........#......"
"#.#...#.#...#.............#...#.....#.#.........#......"
"#.#...#.#...#.............#...#.....#.#.........#......"
"#.###########.............#...#######.#.........#######"
"#.....#.#.................#...........#...............#"
"#.....#.#.....#############...........#########.......#"
"#.....#.#.....#...............................#.......#"
"#########.....#...............................#.......#"
"......#.......#...............................#.......#"
"......#.......#.........................###########...#"
"......#.......#.........................#.....#...#...#"
"......#########.........................#.#############"
"........................................#.#...#...#...."
"........................................#.#...#...#...."
"........................................#.#...#...#...."
"........................................#######...#...."
"..........................................#.......#...."
"..........................................#########...."
-}

