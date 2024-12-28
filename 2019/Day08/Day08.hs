-- Day08.hs
-- AoC 2019 Day 8: Space Image Format
-- Author: Chi-Kit Pao


import Control.Exception (assert)
import Data.List (minimumBy, transpose, intercalate)

-- end is exclusive
slice :: [Char] -> Int -> Int -> [Char]
slice text start end = take (end - start) (drop start text)

count x = length . filter (x==)

part1 :: [[Char]] -> Int
part1 layers = 
    let temp = [[count '0' layer, count '1' layer, count '2' layer] | layer <- layers]
        f lhs rhs = head lhs `compare` head rhs
        t2 = minimumBy f temp in
    (t2 !! 1) * (t2 !! 2)

transformPixel pixels = 
    let pixel = head $ dropWhile (=='2') pixels in
    case pixel of
        '0' -> '.'  -- black
        '1' -> '#'  -- white

part2 :: [[Char]] -> Int -> Int -> [Char]
part2 layers width height = 
    let transposed = transpose layers
        outp = [transformPixel pixels | pixels <- transposed]
        outp2 = [slice outp ((index-1) * width) (index * width) | index <- [1..height]] in
    ---- These three code lines do the same thing, but "\n" is printed instead of a new 
    ---- line when "print" is used. But output is okay when "putStrLn" is used.
    -- concat $ intersperse "\n" outp2
    -- intercalate "\n" outp2
    unlines outp2

main :: IO ()
main = do
    inputLines <- lines <$> readFile "input.txt"
    let dataLine = head inputLines
        lineLength = length dataLine
        width = 25
        height = 6
        layerSize = width * height
        layerCount = assert (lineLength `mod` layerSize == 0) (lineLength `div` layerSize)
        layers = [slice dataLine ((index-1) * layerSize) (index * layerSize) | index <- [1..layerCount]]
    
    putStrLn "Question 1: Find the layer that contains the fewest 0 digits. On that layer, what is the number of 1 digits multiplied by the number of 2 digits?"
    print $ part1 layers

    putStrLn "Question 2: What message is produced after decoding your image?"
    putStrLn $ part2 layers width height

-- Answer 1: 1474
-- Answer 2: JCRCB
-- ..##..##..###...##..###..
-- ...#.#..#.#..#.#..#.#..#.
-- ...#.#....#..#.#....###..
-- ...#.#....###..#....#..#.
-- #..#.#..#.#.#..#..#.#..#.
-- .##...##..#..#..##..###..
