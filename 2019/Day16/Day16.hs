-- Day16.hs
-- AoC 2019 Day 16: Flawed Frequency Transmission
-- Author: Chi-Kit Pao

import Control.Exception (assert)
import Data.Char (ord)

fft :: [Int] -> [[Int]] -> Int -> Int -> [Int]
fft inputListEnd pattern_ currentStep endStep = 
    let inputListEnd' = [ abs (sum $ fmap (uncurry (*)) (zip inputListEnd (pattern_ !! i))) `mod` 10| i <- [0..(length inputListEnd - 1)]] in
    if currentStep >= endStep then inputListEnd' else fft inputListEnd' pattern_ (currentStep + 1) endStep

fft2 :: [Int] -> Int -> Int -> [Int]
fft2 inputList currentStep endStep = 
    let inputList' = scanr1 (\x y -> (x + y) `mod` 10) inputList in
    if currentStep >= endStep then inputList' else fft2 inputList' (currentStep + 1) endStep

part2 :: [Char] -> [Int] -> Int -> [Int]
part2 inputLine inputList endStep = 
    let skipValue = (read :: String -> Int) (take 7 inputLine)
        inputListEnd = drop skipValue $ concat $ replicate 10000 inputList in
    -- The remaining patterns are:
    -- index <skipValue>: [<skipValue> times 0, <total length - skipValue> times 1 ]
    -- index <skipValue + 1>: [<skipValue + 1> times 0, <total length - skipValue - 1> times 1 ]
    -- ...
    -- index <total length - 2> = <skipValue + (total length - skipValue - 2)>: [<total length - 2> times 0, 2 times 1 ]
    -- index <total length - 1> = <skipValue + (total length - skipValue - 1)>: [<total length - 1> times 0, 1 times 1 ]
    fft2 inputListEnd 1 endStep

main :: IO ()
main = do
    inputLines <- lines <$> readFile "input.txt"
    let inputLine = head inputLines
        -- inputLine = "03036732577212944063491565474664"
        inputList = map (\c -> ord c - ord '0') inputLine
        len = length inputLine
        pattern_ = [take len $ drop 1 $ cycle (replicate i 0 ++ replicate i 1 ++ replicate i 0 ++ replicate i (-1)) | i <- [1..len]]
        answer1 = fft inputList pattern_ 1 100 
        answer1'= concatMap show (take 8 answer1)
   
    putStrLn "Question 1: After 100 phases of FFT, what are the first eight digits in the final output list?"
    print $ "Answer 1: " ++ answer1'

    putStrLn "Question 2: After repeating your input signal 10000 times and running 100 phases of FFT, what is the eight-digit message embedded in the final output list?"

    -- Output:
    -- "length inputLine: 650"
    -- "Value to skip: 5975053"
    let skipValue = (read :: String -> Int) (take 7 inputLine)
    -- This assert holds for my input. Algorithm only works when this condition is fulfilled.
    assert(skipValue < (len * 10000) && 2 * skipValue > (len * 10000)) print $ "length inputLine: " ++ show len
    print $ "Value to skip: " ++ show skipValue

    let answer2 = part2 inputLine inputList 100
        answer2' = concatMap show (take 8 answer2)
    print $ "Answer 2: " ++ answer2'

-- Answer 1: 84487724
-- Answer 2: 84692524
