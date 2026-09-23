{-
    Day10.hs
    AoC 2020 Day 10: Adapter Array
    Author: Chi-Kit Pao
    
    Output:
    Question 1: What is the number of 1-jolt differences multiplied by the number of 3-jolt differences?
    Answer: 1914
    Question 2: What is the total number of distinct ways you can arrange the adapters to connect the charging outlet to your device?
    Answer 2: 9256148959232
-}

import Data.List
import Data.List.Split
import qualified Data.Set as Set

-- Add 1 to (joltlist 3) since my device's built-in adapter is always 3 higher than the highest adapter.
answer1 :: [Int] -> Int
answer1 numbers = joltlist 1 * (joltlist 3 + 1)
    where
        lst =  fmap (\[x, y] -> y - x) $ divvy 2 1 $ sort numbers2
        numbers2 = numbers ++ [0]  -- add charging outlet with 0 jolts
        joltlist n = length $ Data.List.filter (== n) lst

-- Construct answer 2 backwards
f :: Set.Set Int -> Int -> [Int] -> Int
f set current lst
    | current == 0 = sum_ 
    | Set.member current set = f set (current - 1) (sum_ : init_)
    | otherwise =  f set (current - 1) (0 : init_)
    where init_ = init lst
          sum_ = sum init_

answer2 :: [Int] -> Int
answer2 numbers = f s m [1, 0, 0, 1]
    where 
        m = maximum numbers
        s = Set.fromList numbers

main :: IO ()
main = do
    numbers <- fmap (read :: String -> Int) . lines <$> readFile "input.txt"

    putStrLn "Question 1: What is the number of 1-jolt differences multiplied by the number of 3-jolt differences?"
    putStrLn $ "Answer: " ++ show (answer1 numbers)
    putStrLn "Question 2: What is the total number of distinct ways you can arrange the adapters to connect the charging outlet to your device?"
    putStrLn $ "Answer 2: " ++ show (answer2 numbers)
