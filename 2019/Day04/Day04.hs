-- Day04.hs
-- AoC 2019 Day 4: Secure Container
-- Author: Chi-Kit Pao

inputLowerBound :: Int
inputLowerBound = 347312
inputUpperBound :: Int
inputUpperBound = 805915
range :: [Int]
range = [inputLowerBound..inputUpperBound]

getDigits :: Int -> [Int]
getDigits 0 = []
getDigits number = let temp = divMod number 10 in
        snd temp : getDigits (fst temp)

getDigitPairs :: [Int] -> [(Int, Int)]
getDigitPairs l 
    | length l <= 1 = []
    | otherwise = (head l , l !! 1) : getDigitPairs (tail l)

checkPairPart2Aux :: [(Int, Int)] -> Int -> Bool
checkPairPart2Aux pairList index
                    | uncurry (/=) currentPair = False
                    | index > 0 && fst (pairList !! (index - 1)) == fst currentPair = False
                    | index < (length pairList - 1) && snd currentPair == snd (pairList !! (index + 1)) = False
                    | otherwise = True
                    where currentPair = pairList !! index

checkPairPart2 :: [(Int, Int)] -> Bool
checkPairPart2 pairList = any (checkPairPart2Aux pairList) [0..(length pairList - 1)]

checkNumber :: Bool ->Int ->  Bool
checkNumber isPart2 number = 
    let digitList = getDigits number
        pairList = getDigitPairs digitList
        check1 = any (uncurry (==))  pairList
        check2 = all (uncurry (>=))  pairList 
        check3 = not isPart2 || checkPairPart2 pairList in
    check1 && check2 && check3

validNumbers1 :: [Int]
validNumbers1 = filter (checkNumber False) range
validNumbers2 :: [Int]
validNumbers2 = filter (checkNumber True) range

main :: IO ()
main = do
    putStrLn "Question 1: How many different passwords within the range given in your puzzle input meet these criteria?"
    print $ length validNumbers1

    putStrLn "Question 2: How many different passwords within the range given in your puzzle input meet all of the criteria?"
    print $ length validNumbers2    

-- Answer1: 594
-- Answer2: 364
