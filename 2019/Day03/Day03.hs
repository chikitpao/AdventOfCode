-- Day03.hs
-- AoC 2019 Day 3: Crossed Wires
-- Author: Chi-Kit Pao

import Data.List
import Data.Maybe
import qualified Data.Set as Set


replaceChar :: [Char] -> Char ->  [Char]
replaceChar str ch = map (\c -> if c==ch then ' '; else c) str

manhattanDistance :: (Int, Int) -> Int
manhattanDistance pos = abs(fst pos) + abs(snd pos)

wirePosFromInstrs :: (Int, Int) -> [String] -> [(Int, Int)]
wirePosFromInstrs _ [] = []
wirePosFromInstrs pos (x:xs) = 
    let prefix = head x
        steps = read (tail x) :: Int in
        case prefix of
            'U' -> let newPos = [(fst pos, snd pos + yPos) | yPos <- [1..steps]] in
                newPos ++ wirePosFromInstrs (last newPos) xs
            'R' -> let newPos = [(fst pos + xPos, snd pos) | xPos <- [1..steps]] in
                newPos ++ wirePosFromInstrs (last newPos) xs
            'D' -> let newPos = [(fst pos, snd pos - yPos) | yPos <- [1..steps]] in
                newPos ++ wirePosFromInstrs (last newPos) xs
            'L' -> let newPos = [(fst pos - xPos, snd pos) | xPos <- [1..steps]] in
                newPos ++ wirePosFromInstrs (last newPos) xs
            _ -> [(0, 0)]

combinedSteps :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]-> (Int, Int, Int)
combinedSteps pos wirePosList1 wirePosList2 =
    let index1 = fromJust $ elemIndex pos wirePosList1
        index2 = fromJust $ elemIndex pos wirePosList2
        steps = index1 + index2 + 2 in
        (fst pos, snd pos, steps)

getStepCount ::  (Int, Int, Int) -> Int
getStepCount st = 
    let (_, _, a) = st in a

main :: IO ()
main = do
    inputLines <- lines <$> readFile "Day03_input.txt"
    let wireInstr1 = words $ replaceChar (head inputLines) ','
        wireInstr2 = words $ replaceChar (inputLines !! 1) ','
        wirePosList1 = wirePosFromInstrs (0, 0) wireInstr1
        wirePosList2 = wirePosFromInstrs (0, 0) wireInstr2
        wirePosSet1 = Set.fromList wirePosList1
        wirePosSet2 = Set.fromList wirePosList2
        inters = Set.intersection wirePosSet1 wirePosSet2
        intersList = Set.toList inters
        
        minValue = minimumBy (\x y -> manhattanDistance x `compare` manhattanDistance y) intersList

        steps = map (\pos -> combinedSteps pos wirePosList1 wirePosList2) intersList
        minStepValue = minimumBy (\x y -> getStepCount x `compare` getStepCount y) steps

    putStrLn "Question 1: What is the Manhattan distance from the central port to the closest intersection?"
    putStr $ show minValue ++ " -> "
    print (manhattanDistance minValue)

    putStrLn "Question 2: What is the fewest combined steps the wires must take to reach an intersection?"
    putStr $ show minStepValue ++ " -> "
    print (getStepCount minStepValue)

-- Answer1: 1431
-- Answer2: 48012
