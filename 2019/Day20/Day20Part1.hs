-- Day20Part1.hs
-- AoC 2019 Day 20: Donut Maze (Part 1)
-- Author: Chi-Kit Pao

import Control.Exception (assert)
import Data.Char (isDigit, isLower, isUpper)
import Data.Function (on)
import Data.List (find, findIndex, sortBy)
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Map as Map


data State = Unvisited | Visiting | Visited deriving (Enum, Show)

data Pos = Pos {
                posRow :: Int
                , posColumn :: Int
                } deriving (Show, Eq, Ord)

data Vertice = Vertice {
                    verticePos :: Pos
                    , neighbors :: [Maybe Pos]
                }
                deriving (Show)
data DijkstraVertice = DijkstraVertice {
                    vertice :: Vertice
                    , distance :: Float
                    , previous :: Maybe Pos
                    , state :: State
                }
                deriving (Show)
data Maze = Maze {
                startPos :: Pos
                , endPos :: Pos
                , vertices :: Map.Map Pos Vertice
            }

findNeighbor :: [[Char]] -> Pos -> Maybe (Char, Pos) -> Maybe Pos
findNeighbor inputLines neighborPos portalExitTile
    | isUpper ((inputLines !! posRow neighborPos) !! posColumn neighborPos) && isJust portalExitTile = Just $ snd $ fromJust portalExitTile 
    | isValidTile ((inputLines !! posRow neighborPos) !! posColumn neighborPos) = Just neighborPos
    | otherwise = Nothing

createVertice :: [[Char]] -> [(Char, Pos)] -> Pos -> Vertice
createVertice inputLines portalTiles pos= 
    let row = posRow pos
        column = posColumn pos
        c = (inputLines !! row) !! column
        portalExitTile = if isPortalTile c then find (\(a, b) -> a == c && b /= pos) portalTiles else Nothing
        northNeighbor = findNeighbor inputLines (Pos (row - 1) column) portalExitTile
        eastNeighbor = findNeighbor inputLines (Pos row (column + 1)) portalExitTile
        southNeighbor = findNeighbor inputLines (Pos (row + 1) column) portalExitTile
        westNeighbor = findNeighbor inputLines (Pos row (column - 1)) portalExitTile in
    Vertice pos [northNeighbor, eastNeighbor, southNeighbor, westNeighbor]

isPortalTile :: Char -> Bool
isPortalTile tile
    | isLower tile = True
    | isDigit tile = True
    | otherwise = False

isValidTile :: Char -> Bool
isValidTile tile
    | isPortalTile tile = True
    | tile == '.' = True
    | otherwise = False

parseInput :: [[Char]] -> Maze
parseInput inputLines = 
    let portalTiles = [ ((inputLines !! row) !! column, Pos row column) | row <- [0..(length inputLines -1)], column <- [0..(length (head inputLines) - 1)]
                    , let c = (inputLines !! row) !! column, isPortalTile c ]
        startPos_ = fromJust $ lookup 'a' portalTiles
        endPos_ = fromJust $ lookup 'z' portalTiles 
        vertices_ = Map.fromList [ (Pos row column, createVertice inputLines portalTiles (Pos row column))| row <- [0..(length inputLines -1)], column <- [0..(length (head inputLines) - 1)]
                    , let c = (inputLines !! row) !! column, isValidTile c]
    in
    Maze startPos_ endPos_ vertices_


handleNeighbor :: Maybe Pos -> DijkstraVertice -> Map.Map Pos DijkstraVertice -> [DijkstraVertice] -> (Map.Map Pos DijkstraVertice, [DijkstraVertice])
handleNeighbor neighborPos currentVertice dijkVertices visiting = 
    if isNothing neighborPos
        then (dijkVertices, visiting)
    else
        let dijkVerticeA = Map.lookup (fromJust neighborPos) dijkVertices
            dijkVertices' = if isNothing dijkVerticeA then dijkVertices else Map.delete (fromJust neighborPos) dijkVertices
            dijkVerticeBIndex = findIndex (\x -> verticePos (vertice x) == fromJust neighborPos) visiting
            dijkVerticeB = if isNothing dijkVerticeBIndex then Nothing else Just (visiting !! fromJust dijkVerticeBIndex)
            nextDistance = assert(isNothing dijkVerticeA || isNothing dijkVerticeB) distance currentVertice + 1.0 in
            if isNothing dijkVerticeA && isNothing dijkVerticeB
                then (dijkVertices', visiting)
            else
                if isJust dijkVerticeA
                    then
                        let newDijkVertice = DijkstraVertice (vertice (fromJust dijkVerticeA)) nextDistance (previous currentVertice) Visiting
                            visiting' = visiting ++ [newDijkVertice] in
                        (dijkVertices', visiting')
                else
                    if nextDistance < distance (fromJust dijkVerticeB)
                        then
                            let index = fromJust dijkVerticeBIndex
                                visiting' =  take index visiting ++ drop (index + 1) visiting
                                newDijkVertice = DijkstraVertice (vertice (fromJust dijkVerticeA)) nextDistance (previous currentVertice) Visiting
                                visiting'' = visiting' ++ [newDijkVertice] in
                            (dijkVertices', visiting'')
                    else
                        (dijkVertices', visiting)
                        

findShortestDistance :: Maze -> Map.Map Pos DijkstraVertice -> [DijkstraVertice] -> Float
findShortestDistance maze dijkVertices visiting =
    -- unvisited DijkstraVertice(s) in map, visiting DijkstraVertice(s) in list
    if null visiting then -1
    else 
        let visiting' = sortBy (compare `on` distance) visiting
            currentVertice = head visiting'
            visiting'' = tail visiting'
            currentVertice' = DijkstraVertice (vertice currentVertice) (distance currentVertice) (previous currentVertice) Visited in
            if verticePos (vertice currentVertice') == endPos maze
                then distance currentVertice'
            else
                let neighbors_ = neighbors $ vertice currentVertice'
                    (dijkVertices1, visiting1) = handleNeighbor (head neighbors_) currentVertice' dijkVertices visiting''
                    (dijkVertices2, visiting2) = handleNeighbor (neighbors_ !! 1) currentVertice' dijkVertices1 visiting1
                    (dijkVertices3, visiting3) = handleNeighbor (neighbors_ !! 2) currentVertice' dijkVertices2 visiting2
                    (dijkVertices4, visiting4) = handleNeighbor (neighbors_ !! 3) currentVertice' dijkVertices3 visiting3 in
                findShortestDistance maze dijkVertices4 visiting4

part1 :: Maze -> Int
part1 maze = 
    let dijkVertices = Map.fromList [ (verticePos v, DijkstraVertice v (read "Infinity") Nothing Unvisited) | v <- Map.elems $ vertices maze]
        startDijkVertice = dijkVertices Map.! startPos maze
        startDijkVertice' = DijkstraVertice (vertice startDijkVertice) 0.0 Nothing Visiting
        dijkVertices' = Map.delete (startPos maze) dijkVertices
        visiting = [startDijkVertice'] in
    round $ findShortestDistance maze dijkVertices' visiting

main :: IO ()
main = do
    -- I've replaced tiles next to the portals with small letters and digits.
    -- AA -> a, YY -> z
    -- OE -> b, LW -> c, ZP -> d, PF -> e, XW -> f, PS -> g, YT -> h, HV -> i,
    -- NP -> j, TZ -> k, SB -> l, WZ -> m, QB -> n, TH -> o, KI -> p, VD -> q,
    -- TG -> r, XG -> s, MU -> t, XX -> u, NJ -> v, KX -> w, SO -> x, XH -> y,
    -- IF -> 0, YH -> 1, TF -> 2.
    inputLines <- lines <$> readFile "input_modified.txt"
    let maze = parseInput inputLines
    putStrLn "Question 1: How many steps does it take to get from the open tile marked AA to the open tile marked ZZ?"
    print $ part1 maze

-- Answer 1: 476
