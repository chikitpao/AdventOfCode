-- Day12.hs
-- AoC 2019 Day 12: The N-Body Problem
-- Author: Chi-Kit Pao

import Data.Char (isDigit)
import Data.List (transpose)

data Moon  = Moon {   x :: Int
                    , y :: Int
                    , z :: Int
                    , vx :: Int
                    , vy :: Int
                    , vz :: Int
                    } deriving (Show)
pot :: Moon -> Int
pot moon = sum [abs(x moon) + abs(y moon) + abs(z moon)]
kin :: Moon -> Int
kin moon = sum [abs(vx moon) + abs(vy moon) + abs(vz moon)]
step :: [Moon] -> Moon -> Moon
step ml m =
    -- Also considered difference with own moon, but it's okay since these
    -- are zero.
    let dvl = [[signum $ x om - x m, signum $ y om - y m, signum $ z om - z m] | om <- ml]
        dv = map sum $ transpose dvl
        newVx = vx m + head dv
        newVy = vy m + dv !! 1 
        newVz = vz m + dv !! 2 in
    Moon (x m + newVx) (y m + newVy) (z m + newVz) newVx newVy newVz

extractInts :: String -> Int
extractInts s = 
    let sl = takeWhile (\c -> isDigit c || c == '-') $ dropWhile (\c -> not(isDigit c) && c /= '-') s in
    read sl::Int

initMoon :: [Int] -> Moon
initMoon pos = Moon (head pos) (pos !! 1) (pos !! 2) 0 0 0

moveMoons :: [Moon] -> Int -> Int -> [Moon]
moveMoons ml previousStep steps =
    let currentStep = previousStep + 1
        newMl = [step ml m | m <- ml] in
    if currentStep >= steps
    then newMl
    else moveMoons newMl currentStep steps

part1 :: [Moon] -> Int
part1 ml = 
    let newMl = moveMoons ml 0 1000
        pots = map pot newMl 
        kins = map kin newMl in
    sum $ zipWith (*) pots kins

main :: IO ()
main = do
    -- Example input:
    -- <x=3, y=3, z=0>
    inputLines <- lines <$> readFile "input.txt"
    let ml = [ initMoon $ map extractInts $ words line | line <- inputLines]
    
    putStrLn "Question 1: What is the total energy in the system after simulating the moons given in your scan for 1000 steps?"
    print $ part1 ml

-- Answer 1: 12351
