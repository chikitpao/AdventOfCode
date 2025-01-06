-- Day16.hs
-- AoC 2019 Day 16: Flawed Frequency Transmission
-- Author: Chi-Kit Pao


import Data.Char (ord)

fft :: [Int] -> [[Int]] -> Int -> Int -> [Int]
fft inputList pattern_ currentStep endStep = 
    let inputList' = [ abs (sum $ fmap (uncurry (*)) (zip inputList (pattern_ !! i))) `mod` 10| i <- [0..(length inputList - 1)]] in
    if currentStep >= endStep then inputList' else fft inputList' pattern_ (currentStep + 1) endStep

main :: IO ()
main = do
    inputLines <- lines <$> readFile "input.txt"
    let inputLine = head inputLines
        inputList = map (\c -> ord c - ord '0') inputLine
        len = length inputLine
        pattern_ = [take len $ drop 1 $ cycle (replicate  i 0 ++ replicate  i 1 ++ replicate  i 0 ++ replicate  i (-1)) | i <- [1..len]]
        answer1 = fft inputList pattern_ 1 100 
        answer1'= concatMap show (take 8 answer1)
   
    putStrLn "Question 1: After 100 phases of FFT, what are the first eight digits in the final output list?"
    print answer1'

-- Answer 1: 84487724

