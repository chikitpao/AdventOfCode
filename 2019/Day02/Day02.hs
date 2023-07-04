-- Day02.hs
-- AoC 2019 Day 2: 1202 Program Alarm
-- Author: Chi-Kit Pao

replaceChar :: [Char] -> Char ->  [Char]
replaceChar str ch = map (\c -> if c==ch then ' '; else c) str

replaceNth :: Int -> Int -> [Int]-> [Int]
replaceNth index value oldList = f ++ (value : tail s)
                               where (f, s) = splitAt index oldList

program :: IO [Int]
program = do
    line <- head . lines <$> readFile "Day02_input.txt"
    return $ map (read::String -> Int) $ words $ replaceChar line ','

modifiedProgram :: Int -> Int -> IO [Int]
modifiedProgram noun verb = do
    program' <- program
    let program'' = replaceNth 1 noun program'
        program''' = replaceNth 2 verb program''
    return program'''

runProgram :: ([Int] , Int) -> Maybe ([Int] , Int)
runProgram (program', line) = 
    let opcode = program' !! line in
    case opcode of
        99 -> Just (program', line) 
        _ -> let len = length program' in
            if (line + 4) > len then
                Nothing
            else
                let firstOperand = program' !! (program' !! (line + 1))
                    secondOperand = program' !! (program' !! (line + 2))
                    targetPos = program' !! (line + 3) in
                case opcode of
                    1 -> runProgram (replaceNth targetPos (firstOperand + secondOperand) program', line + 4)
                    2 -> runProgram (replaceNth targetPos (firstOperand * secondOperand) program', line + 4)
                    _ -> Nothing

tryInputs :: Int -> IO (Maybe Int)
tryInputs value
    |    value < 0 = return Nothing
    | value > 10000 = return Nothing
    | otherwise = do
                modifiedProgram' <- modifiedProgram (value `div` 100) (value `mod` 100)
                let result' = runProgram (modifiedProgram', 0) in
                    case result' of
                        Nothing -> tryInputs (value + 1)
                        Just outp -> let output = head $ fst outp in
                            case output of
                                19690720 -> return (Just value)
                                _ -> tryInputs (value + 1)

main :: IO ()
main = do
    putStrLn "Question 1: What value is left at position 0 after the program halts?"
    modifiedProgram' <- modifiedProgram 12 2
    let result1 = runProgram (modifiedProgram', 0) in
        case result1 of
            Nothing -> print "ERROR: Abnormal abortion"
            Just endProgram-> print $ head $ fst endProgram

    putStrLn "Question 2: Find the input noun and verb that cause the program to produce the output 19690720."
    result2 <- tryInputs 0
    case result2 of
        Nothing -> print "ERROR: No valid input found"
        Just inp -> print inp

-- Answer1: 3516593
-- Answer2: 7749
