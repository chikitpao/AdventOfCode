-- IntCode.hs
-- AoC 2019: Module to handle IntCode programs
-- Author: Chi-Kit Pao
--

module IntCode 
(
replaceChar,
program,
replaceNth,
get1st,
get2nd,
get3rd,
get4th,
runProgram,
runProgram2,
)
where

import Data.Maybe ( fromJust )
import Control.Exception (assert)
import Debug.Trace

-- debug = flip trace

replaceChar :: [Char] -> Char ->  [Char]
replaceChar str ch = map (\c -> if c==ch then ' '; else c) str

program :: [Char] -> IO [Int]
program fileName = do
    line <- head . lines <$> readFile fileName
    return $ map (read::String -> Int) $ words $ replaceChar line ','

replaceNth :: Int -> Int -> [Int]-> [Int]
replaceNth index value oldList = f ++ (value : tail s)
                               where (f, s) = splitAt index oldList

getOpcodeLength :: Int -> Int
getOpcodeLength 1 = 4
getOpcodeLength 2 = 4
getOpcodeLength 3 = 2
getOpcodeLength 4 = 2
getOpcodeLength 5 = 3
getOpcodeLength 6 = 3
getOpcodeLength 7 = 4
getOpcodeLength 8 = 4
getOpcodeLength 99 = 1
getOpcodeLength _ = 10000


checkProgramLength :: Int -> Int -> Int -> Bool
checkProgramLength line programLength opcode = (line + getOpcodeLength opcode) <= programLength

get1st :: (a, b, c, d) -> a
get1st (x, _, _, _) = x

get2nd :: (a, b, c, d) -> b
get2nd (_, x, _, _) = x

get3rd :: (a, b, c, d) -> c
get3rd (_, _, x, _) = x

get4th :: (a, b, c, d) -> d
get4th (_, _, _, x) = x


-- Returns operand address
getOperandAddr :: [Int] -> Int -> Int -> Int -> Int -> Maybe Int
getOperandAddr program opcode mode line pos
    | pos > getOpcodeLength opcode - 1 = Nothing
    | otherwise = let currentMode = (mode `div` 10^(pos - 1)) `mod` 10 in
        case currentMode of
            0 -> Just(program !! (line + pos)) -- position mode
            1 -> Nothing
            _ -> Nothing

-- Returns operand value
getOperandValue :: [Int] -> Int -> Int -> Int -> Int -> Maybe Int
getOperandValue program opcode mode line pos
    | pos > getOpcodeLength opcode - 1 = Nothing
    | otherwise = let currentMode = (mode `div` (10^(pos - 1))) `mod` 10 in
        case currentMode of
            0 -> Just(program !! (program !! (line + pos))) -- position mode
            1 -> Just(program !! (line + pos)) -- immediate mode
            _ -> Nothing

-- debug helper function
helper :: Int -> [Int] -> Int -> String
helper opcode program line
    | getOpcodeLength opcode == 2 = "[" ++ show (program !! (line + 1)) ++ "]"
    | getOpcodeLength opcode == 3 = "[" ++ show (program !! (line + 1)) ++ ", " ++ show(program !! (line + 2)) ++  "]"
    | getOpcodeLength opcode == 4 = "[" ++ show (program !! (line + 1)) ++ ", " ++ show(program !! (line + 2)) ++ ", " ++ show (program !! (line + 3)) ++  "]"

-- debug function
test :: String -> Int -> Int -> Int -> [Int] -> Int -> Maybe Int -> String
test c opcode testOpcode mode program line outputValue
    | opcode == 99 = show line ++ " " ++ show testOpcode ++ "[] " ++ c ++ " " ++ show outputValue ++ " " ++ show mode
    | otherwise = show line ++ " " ++ show testOpcode ++ " " ++ helper opcode program line ++  " " ++ show outputValue ++ " " ++ show mode

runProgram2 :: [Int] -> Int -> Bool -> Maybe [Int] -> Maybe Int -> Maybe ([Int], Int, Maybe [Int], Maybe Int)
runProgram2 program' line haltOnOutput inputValue outputValue = 
    let tempOpcode = program' !! line
        mode = tempOpcode `div` 100
        opcode = tempOpcode `mod` 100 in
        if not (checkProgramLength line (length program') opcode) then
            Nothing
        else if opcode == 99 then
            --Just (program', line, inputValue, outputValue) `debug` test "End" opcode tempOpcode mode program' line outputValue
            Just (program', line, inputValue, outputValue)
        else
            let getOperandAddr' = getOperandAddr program' opcode mode line
                getOperandValue' = getOperandValue program' opcode mode line
                -- newLineValue = (line + getOpcodeLength opcode) `debug` test "Oper" opcode tempOpcode mode program' line outputValue in
                newLineValue = (line + getOpcodeLength opcode) in
            case opcode of
                1 -> runProgram2 (replaceNth (fromJust (getOperandAddr' 3)) (fromJust (getOperandValue' 1) + fromJust (getOperandValue' 2)) program') newLineValue haltOnOutput inputValue outputValue
                2 -> runProgram2 (replaceNth (fromJust (getOperandAddr' 3)) (fromJust (getOperandValue' 1) * fromJust (getOperandValue' 2)) program') newLineValue haltOnOutput inputValue outputValue
                3 -> case inputValue of
                        Nothing -> Nothing
                        Just [] -> Nothing
                        _ -> let h = head $ fromJust inputValue
                                 t = tail $ fromJust inputValue in
                                 runProgram2 (replaceNth (fromJust (getOperandAddr' 1)) h program') newLineValue haltOnOutput (Just t) outputValue
                4 -> let operand1Value = getOperandValue' 1 in 
                    if haltOnOutput then
                        Just(program', newLineValue, inputValue, operand1Value)
                    else
                        runProgram2 program' newLineValue haltOnOutput inputValue operand1Value
                5 -> let operand1Value = fromJust (getOperandValue' 1) in
                        if operand1Value /= 0 then
                            runProgram2 program' (fromJust (getOperandValue' 2)) haltOnOutput inputValue outputValue
                        else
                            runProgram2 program' newLineValue haltOnOutput inputValue outputValue
                6 -> let operand1Value = fromJust (getOperandValue' 1) in
                        if operand1Value == 0 then
                            runProgram2 program' (fromJust (getOperandValue' 2)) haltOnOutput inputValue outputValue
                        else
                            runProgram2 program' newLineValue haltOnOutput inputValue outputValue
                7 -> let operand1Value = fromJust (getOperandValue' 1) 
                         operand2Value = fromJust (getOperandValue' 2) 
                         comparison = fromEnum (operand1Value < operand2Value) in
                         runProgram2 (replaceNth (fromJust (getOperandAddr' 3)) comparison program') newLineValue haltOnOutput inputValue outputValue
                8 -> let operand1Value = fromJust (getOperandValue' 1) 
                         operand2Value = fromJust (getOperandValue' 2) 
                         comparison = fromEnum (operand1Value == operand2Value) in
                         runProgram2 (replaceNth (fromJust (getOperandAddr' 3)) comparison program') newLineValue haltOnOutput inputValue outputValue
                _ -> Nothing

runProgram :: [Int] -> Int -> Maybe [Int] -> Maybe Int -> Maybe ([Int], Int, Maybe [Int], Maybe Int)
runProgram program' line inputValue outputValue = runProgram2 program' line False inputValue outputValue
