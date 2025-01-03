-- Day12.hs
-- AoC 2019 Day 12: The N-Body Problem
-- Author: Chi-Kit Pao

import Data.Char (isDigit)
import Data.List (transpose)
import qualified Data.Set as Set

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

data Axis = Axis { pos1 :: Int
                , pos2 :: Int
                , pos3 :: Int
                , pos4 :: Int
                , velo1 :: Int
                , velo2 :: Int
                , velo3 :: Int
                , velo4 :: Int
            } deriving (Show, Eq, Ord)

getAxis :: [Moon] -> Int -> Axis
getAxis ml axis = 
    case axis of
        1 -> Axis (x (head ml)) (x (ml !! 1)) (x (ml !! 2)) (x (ml !! 3)) (vx (head ml)) (vx (ml !! 1)) (vx (ml !! 2)) (vx (ml !! 3))
        2 -> Axis (y (head ml)) (y (ml !! 1)) (y (ml !! 2)) (y (ml !! 3)) (vy (head ml)) (vy (ml !! 1)) (vy (ml !! 2)) (vy (ml !! 3))
        _ -> Axis (z (head ml)) (z (ml !! 1)) (z (ml !! 2)) (z (ml !! 3)) (vz (head ml)) (vz (ml !! 1)) (vz (ml !! 2)) (vz (ml !! 3))

moveMoons2 :: [Moon] -> Set.Set Axis -> Int -> Int -> Int
moveMoons2 ml set axis previousStep =
    let currentStep = previousStep + 1
        newMl = [step ml m | m <- ml] 
        newAxis = getAxis newMl axis
        isMember = Set.member newAxis set 
        newSet = Set.insert newAxis set in
    if isMember
    then currentStep
    else moveMoons2 newMl newSet axis currentStep

part2 :: [Moon] -> Int -> Int
part2 ml axis = 
    let set = Set.singleton (getAxis ml axis) in
    moveMoons2 ml set axis 0

main :: IO ()
main = do
    -- Example input:
    -- <x=3, y=3, z=0>
    inputLines <- lines <$> readFile "input.txt"
    let ml = [ initMoon $ map extractInts $ words line | line <- inputLines]
    
    putStrLn "Question 1: What is the total energy in the system after simulating the moons given in your scan for 1000 steps?"
    print $ part1 ml
    putStrLn "Question 2: How many steps does it take to reach the first state that exactly matches a previous state?"
    print $ lcm (lcm (part2 ml 0) (part2 ml 1)) (part2 ml 2)

-- Answer 1: 12351
-- Answer 2: 380635029877596
