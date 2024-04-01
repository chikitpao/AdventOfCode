-- Day17.hs
-- AoC 2019 Day 17: Set and Forget
-- Author: Chi-Kit Pao
--
-- REMARK: Uses own module IntCode

import Control.Exception (assert)
import IntCode
import Data.Char (chr)
import Data.Maybe (fromJust)
import Debug.Trace

debug = flip trace

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

main :: IO ()
main = do
    program <- readProgram "Day17_input.txt"
    

    let programState = createProgramState2 program 0 Nothing Nothing 0
        programResult = runProgramWithState programState False 
        output = convertToMap $ fromJust $ stateOutput (fromJust programResult)    
{-
    mapM_ print output
    putStrLn ""
-}
    putStrLn "Question 1: What is the sum of the alignment parameters for the"
    putStrLn " scaffold intersections?"
    print $ countIntersections output

-- Answer1: 7780
-- Answer2: 

