-- Day19.hs
-- AoC 2019 Day 19: Tractor Beam
-- Author: Chi-Kit Pao
--
-- REMARK: Uses own module IntCode

import Control.Exception (assert)
import IntCode
import Data.Char (chr, ord)
import Data.List (elemIndex, find)
import Data.Maybe (fromJust, isNothing, isJust)
import Debug.Trace

debug = flip trace

e3of5 :: (MyProgram, Int, Int, Int, Int) -> Int
e3of5 (_, _, a, _, _) = a
e4of5 :: (MyProgram, Int, Int, Int, Int) -> Int
e4of5 (_, _, _, a, _) = a
e5of5 :: (MyProgram, Int, Int, Int, Int) -> Int
e5of5 (_, _, _, _, a) = a

getBottomLeftX :: MyProgram -> Int -> Int -> Maybe Int
getBottomLeftX program dim y = 
    let res = until fCheck f (program, dim, 0, 0, y)
        failed = e4of5 res > dim
        x = e4of5 res - 1 in
    if failed then Nothing else Just x

f :: (MyProgram, Int, Int, Int, Int) -> (MyProgram, Int, Int, Int, Int)
f (program, dim, output, x, y) = 
    if x >= dim then
        (program, dim, 1, x + 1, y)
    else
        (program, dim, head . fromJust . stateOutput . fromJust $ runProgram2 program 0 False (Just [x, y]) Nothing, x + 1, y)

fCheck :: (MyProgram, Int, Int, Int, Int) -> Bool
fCheck t = e3of5 t == 1

getUpperLeft :: MyProgram-> Int -> Maybe Int -> Int -> Maybe (Int, Int)
getUpperLeft program columnCount blX y = 
    case blX of 
        Nothing -> Nothing
        Just x ->   let upperY = y - 99
                        rightX = x + 99 in
                        if upperY < 0 || rightX >= columnCount then
                            Nothing
                        else 
                            let output = head . fromJust . stateOutput . fromJust $ runProgram2 program 0 False (Just [rightX, upperY]) Nothing in
                            if output == 0 then
                                Nothing
                            else 
                                Just (x, upperY)

checkPositions :: (MyProgram, Int, Int, Int, Int) -> (MyProgram, Int, Int, Int, Int)
checkPositions (program, dim, result, x, y) = 
    if y >= dim then
        (program, dim, 1, 1, y + 1)
    else
        let ul = getUpperLeft program dim (getBottomLeftX program dim y) y in
        case ul of
            Nothing -> (program, dim, 0, x, y + 1)
            Just (resX, resY) -> (program, dim, 1, resX, resY + 1)

main :: IO ()
main = do
    program <- readProgram "Day19_input.txt"

    let part1Data = [ head . fromJust . stateOutput . fromJust $ runProgram2 program 0 False (Just [x, y]) Nothing | x <- [0..49], y <- [0..49]]

    putStrLn "Question 1: How many points are affected by the tractor beam"
    putStrLn " in the 50x50 area closest to the emitter?"
    print $ "Answer 1: " ++ show (sum part1Data)

    putStrLn "Question 2: Find the 100x100 square closest to the emitter that"
    putStrLn " fits entirely within the tractor beam; within that square, find"
    putStrLn " the point closest to the emitter. What value do you get if you"
    putStrLn " take that point's X coordinate, multiply it by 10000, then add"
    putStrLn " the point's Y coordinate?"

    let dim = 10000
        res = until fCheck checkPositions (program, dim, 0, 0, 0)
        failed = e5of5 res > dim
        resX = e4of5 res
        resY = e5of5 res - 1
    if failed then
        print "Answer 2: No Result found"
    else
        print $ "Answer 2: " ++ show (resX  * 10000 + resY)

-- Answer1: 156
-- Answer2: 2610980
