-- IntCode.hs
-- AoC 2019: Module to handle IntCode programs
-- Author: Chi-Kit Pao
--

module IntCode 
(
replaceChar,
readProgram,
setValue,
runProgram,
runProgram2,
runProgram3,
runProgramWithState,
MyProgram,
programCode,
programMemory,
MyProgramState,
stateProgram,
stateNextLine,
stateInput,
stateOutput,
stateRelBase,
stateIsHalted,
stateIsHaltedForInput,
createProgramState,
createProgramState2
)
where

import qualified Data.Map as Map
import Data.Maybe ( fromJust, isNothing )

replaceChar :: [Char] -> Char ->  [Char]
replaceChar str ch = map (\c -> if c==ch then ' '; else c) str

-- Program code (modifiable) and additional memory
data MyProgram  = MyProgram { 
                    programCode :: [Int]
                    , programMemory :: Maybe (Map.Map Int Int)
                    } deriving (Show)


data MyProgramState = MyProgramState {
                     stateProgram :: MyProgram
                     , stateNextLine :: Int
                     , stateInput :: Maybe [Int]
                     , stateOutput :: Maybe [Int]
                     , stateRelBase :: Int
                     , stateIsHaltedForInput :: Bool
                     , stateIsHalted :: Bool
                     } deriving (Show)

createProgramState :: MyProgram -> Int -> Maybe [Int] -> Maybe [Int] -> Int -> Bool -> Bool-> MyProgramState
createProgramState = MyProgramState 

-- Like createProgramState but reset "halted" states
createProgramState2 :: MyProgram -> Int -> Maybe [Int] -> Maybe [Int] -> Int -> MyProgramState
createProgramState2 stateProgram stateNextLine stateInput stateOutput stateRelBase = 
    createProgramState stateProgram stateNextLine stateInput stateOutput stateRelBase False False

readProgram :: [Char] -> IO MyProgram
readProgram fileName = do
    line <- head . lines <$> readFile fileName
    return $ MyProgram (map (read::String -> Int) $ words $ replaceChar line ',') Nothing


setValue :: Int -> Int -> MyProgram-> MyProgram
setValue index value oldProgram = 
    let valuefunc _ = Just value in
    if index >= length (programCode oldProgram) then
        if isNothing (programMemory oldProgram) then
            MyProgram  (programCode oldProgram) (Just (Map.singleton index value))
        else
            MyProgram  (programCode oldProgram) (Just (Map.alter valuefunc index (fromJust $ programMemory oldProgram)))
    else
        let (f, s) = splitAt index (programCode oldProgram) in
        MyProgram (f ++ (value : tail s)) (programMemory oldProgram)

getOpcodeLength :: Int -> Int
getOpcodeLength 1 = 4
getOpcodeLength 2 = 4
getOpcodeLength 3 = 2
getOpcodeLength 4 = 2
getOpcodeLength 5 = 3
getOpcodeLength 6 = 3
getOpcodeLength 7 = 4
getOpcodeLength 8 = 4
getOpcodeLength 9 = 2
getOpcodeLength 99 = 1
getOpcodeLength _ = error "getOpcodeLength: Invalid opcode"


checkProgramLength :: Int -> Int -> Int -> Bool
checkProgramLength line programLength opcode = (line + getOpcodeLength opcode) <= programLength

-- Returns operand address
getOperandAddr :: MyProgram -> Int -> Int -> Int -> Int -> Int -> Maybe Int
getOperandAddr program opcode mode line relBase pos
    | pos > getOpcodeLength opcode - 1 = Nothing
    | otherwise = let currentMode = (mode `div` 10^(pos - 1)) `mod` 10 in
        case currentMode of
            0 -> if line + pos >= length (programCode program) then
                    Just(line + pos)
                 else
                    Just(programCode program !! (line + pos)) -- position mode
            1 -> Nothing
            2 -> let argPos = programCode program !! (line + pos) in
                if relBase + argPos >= length (programCode program) then
                    Just(relBase  + argPos)
                else
                    Just(programCode program !! (relBase + argPos)) -- relative modes
            _ -> Nothing

-- Returns operand value
getOperandValue :: MyProgram -> Int -> Int -> Int -> Int -> Int -> Maybe Int
getOperandValue program opcode mode line relBase pos
    | pos > getOpcodeLength opcode - 1 = Nothing
    | otherwise = let currentMode = (mode `div` (10^(pos - 1))) `mod` 10 in
        case currentMode of
            -- position mode
            0 -> let actualPos = fromJust $ getOperandAddr program opcode mode line relBase pos in
                    if actualPos >= length (programCode program) then
                        if Map.member actualPos (fromJust (programMemory program)) then
                            Just(fromJust (programMemory program) Map.! actualPos)
                        else
                            -- error ("getOperandValue position mode " ++ show actualPos)
                            -- Read default 0
                            Just 0
                    else
                        Just(programCode program !! actualPos)
            -- immediate mode
            1 -> if line + pos >= length (programCode program) then
                    if Map.member (line + pos) (fromJust (programMemory program)) then
                        Just(fromJust (programMemory program) Map.! (line + pos))
                    else
                        -- error ("getOperandValue immediate mode " ++ show (line + pos))
                        -- Read default 0
                        Just 0
                 else
                    Just(programCode program !! (line + pos))
            -- relative mode
            2 -> let actualPos = fromJust $ getOperandAddr program opcode mode line relBase pos in
                    if actualPos >= length (programCode program) then
                        if Map.member actualPos (fromJust (programMemory program)) then
                            Just(fromJust (programMemory program) Map.! actualPos)
                        else
                            -- error ("getOperandValue relative mode " ++ show actualPos ++ " RelBase " ++ show relBase)
                            -- Read default 0
                            Just 0
                    else
                        Just(programCode program !! actualPos)
            _ -> Nothing

{-
-- debug helper function
helper :: Int -> [Int] -> Int -> String
helper opcode program line
    | getOpcodeLength opcode == 2 = "[" ++ show (program !! (line + 1)) ++ "]"
    | getOpcodeLength opcode == 3 = "[" ++ show (program !! (line + 1)) ++ ", " ++ show(program !! (line + 2)) ++  "]"
    | getOpcodeLength opcode == 4 = "[" ++ show (program !! (line + 1)) ++ ", " ++ show(program !! (line + 2)) ++ ", " ++ show (program !! (line + 3)) ++  "]"
    | otherwise = error "helper: Not implemented for opcode"

-- debug function
test :: String -> Int -> Int -> Int -> MyProgram -> Int -> Int -> Maybe [Int] -> String
test c opcode testOpcode mode program line relBase outputValue
    | opcode == 99 = show line ++ " " ++ show testOpcode ++ "[] " ++ c ++ " " ++ show outputValue ++ " " ++ show mode
    | otherwise = show line ++ " " ++ show testOpcode ++ " " ++ helper opcode (programCode program) line ++  " " ++ show outputValue ++ " " ++ show mode ++ " " ++ show relBase
-}

-- runProgram3: run program, specify haltOnOutput and relBase explicitly
runProgram3 :: MyProgram -> Int -> Bool -> Int -> Maybe [Int] -> Maybe [Int] -> Maybe MyProgramState
runProgram3 program line haltOnOutput relBase inputValue outputValue = 
    let tempOpcode = programCode program !! line
        mode = tempOpcode `div` 100
        opcode = tempOpcode `mod` 100 in
        if not (checkProgramLength line (length $ programCode program) opcode) then
            error "Instruction Pointer out of bound"
        else if opcode == 99 then
            Just (MyProgramState program line inputValue outputValue relBase False True)
        else
            let getOperandAddr' = getOperandAddr program opcode mode line relBase
                getOperandValue' = getOperandValue program opcode mode line relBase
                newLineValue = (line + getOpcodeLength opcode) in
            case opcode of 
                1 -> let op1 = fromJust (getOperandValue' 1)
                         op2 = fromJust (getOperandValue' 2)
                         sum_ = op1 + op2 in
                         runProgram3 (setValue (fromJust (getOperandAddr' 3)) sum_ program) newLineValue haltOnOutput relBase inputValue outputValue 
                2 -> let op1 = fromJust (getOperandValue' 1)
                         op2 = fromJust (getOperandValue' 2)
                         product_ = op1 * op2 in
                         runProgram3 (setValue (fromJust (getOperandAddr' 3)) product_ program) newLineValue haltOnOutput relBase inputValue outputValue
                3 -> case inputValue of
                        Nothing -> Nothing
                        Just [] -> Just (MyProgramState program line inputValue outputValue relBase True False)
                        _ -> let h = head $ fromJust inputValue
                                 t = tail $ fromJust inputValue in
                                 runProgram3 (setValue (fromJust (getOperandAddr' 1)) h program) newLineValue haltOnOutput relBase (Just t) outputValue
                4 -> let operand1Value = getOperandValue' 1 
                         newOutputValue = if isNothing outputValue then Just[fromJust operand1Value] else (++) <$> outputValue <*> Just[fromJust operand1Value] in 
                    if haltOnOutput then
                        Just (MyProgramState program newLineValue inputValue newOutputValue relBase False False)
                    else
                        runProgram3 program newLineValue haltOnOutput relBase inputValue newOutputValue
                5 -> let operand1Value = fromJust (getOperandValue' 1) in
                        if operand1Value /= 0 then
                            runProgram3 program (fromJust (getOperandValue' 2)) haltOnOutput relBase inputValue outputValue
                        else
                            runProgram3 program newLineValue haltOnOutput relBase inputValue outputValue
                6 -> let operand1Value = fromJust (getOperandValue' 1) in
                        if operand1Value == 0 then
                            runProgram3 program (fromJust (getOperandValue' 2)) haltOnOutput relBase inputValue outputValue
                        else
                            runProgram3 program newLineValue haltOnOutput relBase inputValue outputValue
                7 -> let operand1Value = fromJust (getOperandValue' 1) 
                         operand2Value = fromJust (getOperandValue' 2) 
                         comparison = fromEnum (operand1Value < operand2Value) in
                         runProgram3 (setValue (fromJust (getOperandAddr' 3)) comparison program) newLineValue haltOnOutput relBase inputValue outputValue
                8 -> let operand1Value = fromJust (getOperandValue' 1) 
                         operand2Value = fromJust (getOperandValue' 2) 
                         comparison = fromEnum (operand1Value == operand2Value) in
                         runProgram3 (setValue (fromJust (getOperandAddr' 3)) comparison program) newLineValue haltOnOutput relBase inputValue outputValue
                9 -> runProgram3 program newLineValue haltOnOutput (relBase + fromJust (getOperandValue' 1)) inputValue outputValue
                _ -> Nothing

-- runProgram2: run program (specify haltOnOutput explcitly, use default relBase)
runProgram2 :: MyProgram -> Int -> Bool -> Maybe [Int] -> Maybe [Int] -> Maybe MyProgramState
runProgram2 program line haltOnOutput = runProgram3 program line haltOnOutput 0

-- runProgram: run program (use default haltOnOutput and relBase)
runProgram :: MyProgram -> Int -> Maybe [Int] -> Maybe [Int] -> Maybe MyProgramState
runProgram program line = runProgram2 program line False

-- runProgramWithState: run program with state parameter, specify haltOnOutput explicitly
runProgramWithState :: MyProgramState -> Bool -> Maybe MyProgramState
runProgramWithState programState haltOnOutput =
    let program = stateProgram programState
        nextLine = stateNextLine programState
        input = stateInput programState 
        output = stateOutput programState
        relBase = stateRelBase programState in
    runProgram3 program nextLine haltOnOutput relBase input output