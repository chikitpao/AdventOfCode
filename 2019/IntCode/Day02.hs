-- Day02.hs
-- AoC 2019 Day 2: 1202 Program Alarm
-- Author: Chi-Kit Pao
--
-- REMARK: Uses own module IntCode

import IntCode


modifiedProgram :: Int -> Int -> IO [Int]
modifiedProgram noun verb = do
    program' <- program "Day02_input.txt"
    let program'' = replaceNth 1 noun program'
        program''' = replaceNth 2 verb program''
    return program'''

tryInputs :: Int -> IO (Maybe Int)
tryInputs value
    |    value < 0 = return Nothing
    | value > 10000 = return Nothing
    | otherwise = do
                modifiedProgram' <- modifiedProgram (value `div` 100) (value `mod` 100)
                let result' = runProgram modifiedProgram' 0 Nothing Nothing in
                    case result' of
                        Nothing -> tryInputs (value + 1)
                        Just outp -> let output = head $ get1st outp in
                            case output of
                                19690720 -> return (Just value)
                                _ -> tryInputs (value + 1)

main :: IO ()
main = do
    putStrLn "Question 1: What value is left at position 0 after the program halts?"
    modifiedProgram' <- modifiedProgram 12 2
    let result1 = runProgram modifiedProgram' 0 Nothing Nothing in
        case result1 of
            Nothing -> print "ERROR: Abnormal abortion"
            Just endProgram-> print $ head $ get1st endProgram

    putStrLn "Question 2: Find the input noun and verb that cause the program to produce the output 19690720."
    result2 <- tryInputs 0
    case result2 of
        Nothing -> print "ERROR: No valid input found"
        Just inp -> print inp

-- Answer1: 3516593
-- Answer2: 7749
