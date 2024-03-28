-- Day07.hs
-- AoC 2019 Day 7: Amplification Circuit
-- Author: Chi-Kit Pao
--
-- REMARK: Uses own module IntCode

import Control.Exception (assert)
import Data.Function (on)
import Data.List (maximumBy, permutations)
import Data.Maybe ( fromJust, isNothing)
import IntCode
import Debug.Trace

-- debug = flip trace


pairToList :: (a, a) -> [a]
pairToList (x,y) = [x,y]


thruster :: MyProgram -> [Int] -> Int -> Int -> Int -> ([Int], Int)
thruster program input input2 current end = 
    let result = runProgram program 0 (Just [input !! current, input2]) Nothing in
    case result of
    Nothing -> (input, -1)
    Just endProgram -> if current + 1 == end then
                            (input, fromJust $ stateOutput endProgram)
                        else
                            thruster program input (fromJust $ stateOutput endProgram) (current + 1) end


thrusterChain :: MyProgram -> [Int] -> ([Int], Int)
thrusterChain program input = thruster program input 0 0 5

replaceProgramState :: Maybe[MyProgramState] -> Int -> MyProgramState -> Maybe[MyProgramState]
replaceProgramState oldProgramStates current newProgramState =
    if isNothing oldProgramStates then
        Nothing
    else
        Just (f ++ (newProgramState : tail s)) 
        where 
        oldList = fromJust oldProgramStates
        (f, s) = splitAt current oldList


thrusterChain2_CheckResult :: [Int] -> Maybe[MyProgramState] -> Int -> Int -> Maybe MyProgramState -> ([Int], Int)
thrusterChain2_CheckResult input programStates current end result = 
    if isNothing result then
        (input, -1)
    else 
        let endProgram = fromJust result in
        if current + 1 == end then
            let program = stateProgram endProgram
                code = programCode program
                nextLine = stateNextLine endProgram in 
                if (code !! nextLine) `mod` 100 == 99 then
                    (input, fromJust $ stateOutput endProgram)
                else
                    let newCurrent = 0
                        oldProgramState1 = fromJust programStates !! current
                        newProgramStates1 = replaceProgramState programStates current (createProgramState program nextLine (stateInput endProgram) (stateOutput endProgram) (stateRelBase endProgram))
                        oldProgramState2 = fromJust programStates !! newCurrent
                        newProgramStates2 = replaceProgramState newProgramStates1 newCurrent (createProgramState (stateProgram oldProgramState2) (stateNextLine oldProgramState2) ((++) <$> stateInput oldProgramState2 <*> Just [fromJust $ stateOutput endProgram]) (stateOutput oldProgramState2) (stateRelBase oldProgramState2)) in
                    thrusterChain2 program input newProgramStates2 newCurrent end
        else
            let newCurrent = current + 1
                oldProgramState1 = fromJust programStates !! current
                newProgramStates1 = replaceProgramState programStates current (createProgramState (stateProgram endProgram) (stateNextLine endProgram) (stateInput endProgram) (stateOutput endProgram) (stateRelBase endProgram))
                oldProgramState2 = fromJust programStates !! newCurrent
                newProgramStates2 = replaceProgramState newProgramStates1 newCurrent (createProgramState (stateProgram oldProgramState2) (stateNextLine oldProgramState2) ((++) <$> stateInput oldProgramState2 <*> Just [fromJust $ stateOutput endProgram]) (stateOutput oldProgramState2) (stateRelBase oldProgramState2)) in
            thrusterChain2 (stateProgram endProgram) input newProgramStates2 newCurrent end


getStartInput :: [Int] -> Int -> [Int]
getStartInput input index
    | index == 0 = [head input, 0]
    | otherwise = [input !! index]


thrusterChain2 :: MyProgram -> [Int] -> Maybe[MyProgramState] -> Int -> Int -> ([Int], Int)
thrusterChain2 program input programStates current end = 
    if isNothing programStates then
        let newProgramStates = [createProgramState program 0 (Just $ getStartInput input i) Nothing 0 | i <-[0..end-1]]
            newProgramState = newProgramStates !! current
            result = runProgram2 program 0 True (stateInput newProgramState) Nothing in
            thrusterChain2_CheckResult input (Just newProgramStates) current end result
    else
        let newProgramState = fromJust programStates !! current
            result = runProgram2 (stateProgram newProgramState) (stateNextLine newProgramState) True (stateInput newProgramState) (stateOutput newProgramState) in
            thrusterChain2_CheckResult input programStates current end result


part1 :: MyProgram -> ([Int], Int)
part1 program = maximumBy (compare `on` snd) [thrusterChain program p  | p <- permutations [0..4]]


part2 :: MyProgram -> ([Int], Int)
part2 program = maximumBy (compare `on` snd) [thrusterChain2 program p Nothing 0 5  | p <- permutations [5..9]]


main :: IO ()
main = do
    putStrLn "Question 1: Try every combination of phase settings on the"
    putStrLn " amplifiers. What is the highest signal that can be sent to the"
    putStrLn" thrusters?"
    program <- readProgram "Day07_input.txt"
    print $ part1 program -- Output: ([0,3,2,4,1],38500)
    putStrLn "Question 2: Try every combination of the new phase settings on"
    putStrLn " the amplifier feedback loop. What is the highest signal that"
    putStrLn" can be sent to the thrusters?"
    print $ part2 program -- Output: ([7,5,9,6,8],33660560)

-- Answer1: 38500
-- Answer2: 33660560
