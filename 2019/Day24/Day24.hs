-- Day24.hs
-- AoC 2019 Day 24: Planet of Discord
-- Author: Chi-Kit Pao

import Data.List (elemIndices)
import Data.List.Split (chunksOf)
import qualified Data.Map as Map
import qualified Data.Set as Set


-- Begin of part 1
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

part1 :: [String] -> Int -> Int -> Set.Set Int -> Int
part1 area rowCount columnCount history = 
    let newArea = updateArea area rowCount columnCount
        newBiodiversity = biodiversity newArea rowCount columnCount in
    if Set.member newBiodiversity history
        then newBiodiversity
    else
        let newHistory = Set.insert newBiodiversity history in
        part1 newArea rowCount columnCount newHistory
-- End of part 1

-- Begin of part 2
-- Level: list of length 26, entry 0 and 13 are not used.
newtype Level = Level {levelValues :: [Bool]}
instance Show Level where
  show (Level a) = "Level" ++ show (elemIndices True a)

-- ND -> NeighborDef
data ND = ND {
                levelOffset :: Int
                , levelPos :: Int
            }
neighborDefs :: Int -> [ND]
neighborDefs number = 
    case number of
    1 -> [ND (-1) 8, ND 0 2, ND 0 6, ND (-1) 12]
    2 -> [ND (-1) 8, ND 0 3, ND 0 7, ND 0 1]
    3 -> [ND (-1) 8, ND 0 4, ND 0 8, ND 0 2]
    4 -> [ND (-1) 8, ND 0 5, ND 0 9, ND 0 3]
    5 -> [ND (-1) 8, ND (-1) 14, ND 0 10, ND 0 4]
    6 -> [ND 0 1, ND 0 7, ND 0 11, ND (-1) 12]
    7 -> [ND 0 2, ND 0 8, ND 0 12, ND 0 6]
    8 -> [ND 0 3, ND 0 9, ND 1 1, ND 1 2, ND 1 3, ND 1 4, ND 1 5, ND 0 7]
    9 -> [ND 0 4, ND 0 10, ND 0 14, ND 0 8]
    10 -> [ND 0 5, ND (-1) 14, ND 0 15, ND 0 9]
    11 -> [ND 0 6, ND 0 12, ND 0 16, ND (-1) 12]
    12 -> [ND 0 7, ND 1 1, ND 1 6, ND 1 11, ND 1 16, ND 1 21, ND 0 17, ND 0 11]
    14 -> [ND 0 9, ND 0 15, ND 0 19, ND 1 5, ND 1 10, ND 1 15, ND 1 20, ND 1 25]
    15 -> [ND 0 10, ND (-1) 14, ND 0 20, ND 0 14]
    16 -> [ND 0 11, ND 0 17, ND 0 21, ND (-1) 12]
    17 -> [ND 0 12, ND 0 18, ND 0 22, ND 0 16]
    18 -> [ND 1 21, ND 1 22, ND 1 23, ND 1 24, ND 1 25, ND 0 19, ND 0 23, ND 0 17]
    19 -> [ND 0 14, ND 0 20, ND 0 24, ND 0 18]
    20 -> [ND 0 15, ND (-1) 14, ND 0 25, ND 0 19]
    21 -> [ND 0 16, ND 0 22, ND (-1) 18, ND (-1) 12]
    22 -> [ND 0 17, ND 0 23, ND (-1) 18, ND 0 21]
    23 -> [ND 0 18, ND 0 24, ND (-1) 18, ND 0 22]
    24 -> [ND 0 19, ND 0 25, ND (-1) 18, ND 0 23]
    25 -> [ND 0 20, ND (-1) 14, ND (-1) 18, ND 0 24]
    _ -> error $ "neighborDef: Invalid number " ++ show number

queryState :: Map.Map Int Level -> (Int, Int) -> Bool
queryState area2 (level, pos) = Map.member level area2 && (levelValues (area2 Map.! level) !! pos)

addBug :: Map.Map Int Level -> (Int, Int) -> Map.Map Int Level
addBug area2 (level, pos) =
    if Map.member level area2
        then Map.insert level (Level [i == pos || (i /= pos && c) | i <- [0..25], let c = levelValues (area2 Map.! level) !! i] ) area2
    else
        Map.insert level (Level [i == pos | i <- [0..25]]) area2

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

part2 :: Map.Map Int Level -> Int -> Int -> Int
part2 area2 currentStep endStep =
    let area2' = Map.empty 
        assocs_ = Map.assocs area2 
        bugs = [(level, pos) | (level, values) <- assocs_, pos <- [0..25], levelValues values !! pos]
        bugWithNeighbors = [((level, pos) , (\x -> (levelOffset x + level, levelPos x)) <$> neighborDefs pos) | (level, pos) <- bugs]

        -- bugs -> bugs
        newBugs1 = map fst $ filter (\x -> count True (fmap (queryState area2) (snd x)) == 1) bugWithNeighbors
        area2'' = foldl addBug area2' newBugs1
        -- empty -> bugs
        emptyNeighbors = Set.toList . Set.fromList $ filter (not . queryState area2) $ concatMap snd bugWithNeighbors
        emptyNeighborsWithNeighbors = [((level, pos) , (\x -> (levelOffset x + level, levelPos x)) <$> neighborDefs pos) | (level, pos) <- emptyNeighbors]
        newBugs2 = map fst $ filter (\x -> count True (fmap (queryState area2) (snd x)) `elem` [1, 2]) emptyNeighborsWithNeighbors
        area2''' = foldl addBug area2'' newBugs2 in
    
    if currentStep >= endStep
        then sum $ fmap (count True . levelValues) (Map.elems area2''')
    else
        part2 area2''' (currentStep + 1) endStep
-- End of part 2

main :: IO ()
main = do
    inputLines <- lines <$> readFile "input.txt"
    let rowCount = length inputLines
        columnCount = length $ head inputLines
        history = Set.singleton (biodiversity inputLines rowCount columnCount)
        -- part2
        firstLevel = Level [v | i <- [0..25], let v = notElem i [0, 13] && ((inputLines !! ((i - 1) `div` columnCount)) !! ((i - 1) `mod` columnCount)) == occupiedTile]
        area2 = Map.singleton 0 firstLevel
    
    putStrLn "Question 1: What is the biodiversity rating for the first layout that appears twice?"
    print $ part1 inputLines rowCount columnCount history

    putStrLn "Question 2: How many bugs are present after 200 minutes?"
    print $ part2 area2 1 200

-- Answer 1: 28717468
-- Answer 2: 2014
