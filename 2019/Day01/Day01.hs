-- Day01.hs
-- AoC 2019 Day 1: The Tyranny of the Rocket Equation
-- Author: Chi-Kit Pao

calcFuel :: Integer -> Integer
calcFuel x
    | temp <= 0 = 0
    | otherwise = temp
    where temp = x `div` 3 - 2

calcFuel2 :: Integer -> Integer
calcFuel2 x
    | x <= 0 = 0
    | otherwise = calcFuel x + calcFuel2 (calcFuel x)

main = do
    masses <- map read . lines <$> readFile "Day01_input.txt" :: IO [Integer]
    putStrLn "Question 1: What is the sum of the fuel requirements for all of the modules on your spacecraft?"
    print $ sum (map calcFuel masses)
    putStrLn "Question 2: What is the sum of the fuel requirements for all of the modules on your spacecraft when also taking into account the mass of the added fuel?"
    print $ sum (map calcFuel2 masses)
-- Answer1: 3368364
-- Answer2: 5049684
