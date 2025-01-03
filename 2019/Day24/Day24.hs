-- Day24.hs
-- AoC 2019 Day 24: Planet of Discord
-- Author: Chi-Kit Pao

import Data.List (elemIndices)
import Data.List.Split (chunksOf)
import qualified Data.Map as Map

emptyTile :: Char
emptyTile = '.'
occupiedTile :: Char
occupiedTile = '#'

biodiversity :: [String] -> Int -> Int -> Int
biodiversity area rowCount columnCount = 
    sum [ 2^(row * columnCount + column) | row <- [0..rowCount-1], column <- [0..columnCount-1], let c = (area !! row) !! column, c == occupiedTile ]

updateTile :: [String] -> Int -> Int -> Int -> Int -> Char
updateTile area rowCount columnCount row column = 
    let currentTile = (area !! row) !! column
        northTile = if row == 0 then emptyTile else (area !! (row - 1)) !! column
        eastTile = if column + 1 == columnCount then emptyTile else (area !! row) !! (column + 1)
        southTile = if row + 1 == rowCount then emptyTile else (area !! (row + 1)) !! column
        westTile = if column == 0 then emptyTile else (area !! row) !! (column - 1)
        neighborCount = length $ elemIndices occupiedTile [northTile, eastTile, southTile, westTile] in
    if currentTile == occupiedTile
        then if neighborCount /= 1 then emptyTile else occupiedTile
    else
        if neighborCount `elem` [1, 2] then occupiedTile else emptyTile

updateArea :: [String] -> Int -> Int -> [String]
updateArea area rowCount columnCount = 
    chunksOf columnCount [ updateTile area rowCount columnCount row column | row <- [0..rowCount-1], column <- [0..columnCount-1]]

part1 :: [String] -> Int -> Int -> Map.Map Int Int -> Int -> Int
part1 area rowCount columnCount history currentStep = 
    let newArea = updateArea area rowCount columnCount
        newBiodiversity = biodiversity newArea rowCount columnCount in
    if Map.member newBiodiversity history
        then newBiodiversity
    else
        let newHistory = Map.insert newBiodiversity currentStep history in
        part1 newArea rowCount columnCount newHistory (currentStep + 1)

main :: IO ()
main = do
    inputLines <- lines <$> readFile "input.txt"
    let rowCount = length inputLines
        columnCount = length $ head inputLines
        history = Map.singleton (biodiversity inputLines rowCount columnCount) 0
    
    putStrLn "Question 1: What is the biodiversity rating for the first layout that appears twice?"
    print $ part1 inputLines rowCount columnCount history 1

-- Answer 1: 28717468
