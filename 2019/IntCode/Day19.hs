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

getUpperLeft :: [(Int, [Int])]-> Int -> Maybe Int -> Int -> Maybe (Int, Int)
getUpperLeft lines columnCount blX y = 
    case blX `debug` ("y " ++ show y) of
        Nothing -> Nothing
        Just x ->   let upperY = y - 99
                        rightX = x + 99 in
                        if upperY < 0 || rightX >= columnCount || ((snd (lines !! upperY) !! rightX) == 0) then 
                            Nothing
                        else 
                            Just (x, upperY)


getMapLine :: MyProgram -> Int -> Int -> [Int]
getMapLine program y dim = [ head . fromJust . stateOutput . fromJust $ runProgram2 program 0 False (Just [x, y]) Nothing | x <- [0..dim-1]]

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

--TODO
{-
    let dim = 10000
        lines = [(y, getMapLine program y dim) | y <- [0..dim-1]]
        columnCount = length (head lines)
        answer2 = find isJust [getUpperLeft lines columnCount (elemIndex 1 line) y | (y, line) <- lines]
    print $ "Answer 2: " ++ show answer2
-}

-- Answer1: 156
-- Answer2: 
