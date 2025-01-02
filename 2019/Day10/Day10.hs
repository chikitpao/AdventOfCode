-- Day10.hs
-- AoC 2019 Day 10: Monitoring Station
-- Author: Chi-Kit Pao


import Data.Function (on)
import Data.List (maximumBy, sortBy, transpose)
import qualified Data.Map as Map
import qualified Data.Set as Set

xpos :: (a, b) -> a
xpos (x, _) = x
ypos :: (a, b) -> b
ypos (_, y) = y

getBearing :: (Int, Int) -> (Int, Int) -> ((Int, Int), (Int, Int))
getBearing s d = 
    let dx = xpos d - xpos s
        dy = ypos d - ypos s
        cdxdy = case (dx, dy) of
                    (0, 0) -> (0, 0)
                    (0, dy_) -> (0, signum dy_)
                    (dx_, 0) -> (signum dx_, 0)
                    _ -> (dx `div` abs(gcd dx dy), dy `div` abs(gcd dx dy)) in
    (cdxdy, (dx, dy))

getDetectionCount :: Set.Set (Int, Int) -> (Int, Int) -> Int
getDetectionCount set s = 
    let ml = [ getBearing s d | d <- Set.toList set]
        map_ = Map.fromList ml in
    Map.size map_

-- Returns [detection Count, xpos, ypos]
part1 :: Set.Set (Int, Int) -> [Int]
part1 set =
    let dc = [ [getDetectionCount set s, xpos s, ypos s] | s <- Set.toList set]
        f lhs rhs = head lhs `compare` head rhs in
    maximumBy f dc

getBearing2 :: (Int, Int) -> (Int, Int) -> Float
getBearing2 s d = 
    let dx = xpos d - xpos s
        dy = ypos d - ypos s 
        -- Call "atan2 dx -dy" instead of "atan2 dx dy" because:
        -- 0° -> -y
        -- 90° -> x
        result = case (dx, dy) of
                    (0, 0) -> 0.0
                    (0, dy_) -> atan2 0.0 (fromIntegral $ -signum dy_)
                    (dx_, 0) -> atan2 (fromIntegral $ signum dx_) 0.0
                    _ -> atan2 (fromIntegral (dx `div` abs(gcd dx dy))) (-fromIntegral (dy `div` abs(gcd dx dy))) in
        if result < 0.0 
            then result + pi * 2
            else result

getDistance  :: (Int, Int) -> (Int, Int) -> Float
getDistance s d = sqrt $ fromIntegral ((xpos d - xpos s)^(2::Int) + (ypos d - ypos s)^(2::Int) )

data Asteroid  = Asteroid { 
                    bearing :: Float
                    , distance :: Float
                    , x :: Int
                    , y :: Int
                    } deriving (Show)

addToMap :: Map.Map Float [Asteroid] -> [Asteroid] -> Map.Map Float [Asteroid]
addToMap m [] = m
addToMap m (x:xs) = 
    let key = bearing x
        lookupResult = Map.lookup key m in
    case lookupResult of
        Nothing ->
            let m1 = Map.insert key [x] m in
            addToMap m1 xs
        Just lur -> 
            let newList = sortBy (compare `on` distance) (lur ++ [x])
                m1 = Map.update (\_-> Just newList) key m in
            addToMap m1 xs

part2 :: Set.Set (Int, Int) -> (Int, Int) -> (Int, Int)
part2 set stationPos = 
    let al = [ Asteroid (getBearing2 stationPos d) (getDistance stationPos d) (xpos d) (ypos d) | d <- filter (/= stationPos) (Set.toList set)]
        bearingList = Set.toAscList . Set.fromList $ [bearing a | a <- al]
        map_ = addToMap Map.empty al
        al2 = concat $ transpose [ map_ Map.! b | b <- bearingList]
        asteroid = al2 !! 199 in
    (x asteroid, y asteroid)

main :: IO ()
main = do
    inputLines <- lines <$> readFile "input.txt"
    let width = length $ head inputLines
        height = length inputLines
        sl = [ (x, y) | x <- [0..(width-1)], y <- [0..(height-1)], let v = (inputLines !! y) !! x, v == '#']
        set = Set.fromList sl
    
    putStrLn "Question 1: Find the best location for a new monitoring station. How many other asteroids can be detected from that location?"
    let result1 = part1 set
        detected = head result1 - 1 -- minus own position
        stationPos = (result1 !! 1, result1 !! 2)
    print $ show detected ++ " " ++ show stationPos

    putStrLn "Question 2: Determine the 200th asteroid to be vaporized. What do you get if you multiply its X coordinate by 100 and then add its Y coordinate?"
    let result2 = part2 set stationPos
        answer2 = fst result2 * 100 + snd result2
    print $ show answer2 ++ " " ++ show result2

-- Answer 1: 280 (20,18)
-- Answer 2: 706 (7,6)
