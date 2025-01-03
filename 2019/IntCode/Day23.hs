-- Day23.hs
-- AoC 2019 Day 23: Category Six
-- Author: Chi-Kit Pao
--
-- REMARK: Uses own module IntCode

import Control.Exception (assert)
import IntCode
import Data.List (findIndices)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust, isNothing, isJust)


data Result = Result{
                answer1 :: Maybe Int
                , answer2 :: Maybe Int
                } deriving (Show)

data Computer = Computer {
                     computerNumber:: Int
                     , computerProgramState :: MyProgramState
                     , computerMessageQueue :: [Int]
                     } deriving (Show)
data ComputerResult = ComputerResult{
                  resultIsIdle :: Bool
                , outputQueue :: [Packet]
                }
data Packet = Packet {
                packetDst :: Int
                , packetX :: Int
                , packetY :: Int
                } deriving (Show)

checkResult :: ([Computer], Result, Maybe Packet, [Packet]) -> Bool
checkResult (_, result, _, _) = isJust (answer1 result) && isJust (answer2 result)

parseProgramOutput :: Maybe [Int] -> ([Packet], Maybe [Int])
parseProgramOutput output = 
    if isNothing output
        then ([], Just [])
    else
        let len = length $ fromJust output
            q = len `div` 3 
            sendPacketsList = [ Packet (head t) (t !! 1) (t !! 2) | t <- chunksOf 3 (take (q * 3) (fromJust output))]
            outputList = drop (q * 3) (fromJust output) in
    (sendPacketsList, Just outputList)

calculateComputer :: Computer -> (Computer,  ComputerResult)
calculateComputer computer =
    let messageQueue = computerMessageQueue computer
        (sendPackets_, newOutput) = parseProgramOutput (stateOutput $ computerProgramState computer) in
    -- Receive packets
    if null messageQueue
        then let programState = computerProgramState computer
                 newProgramState = createProgramState2 (stateProgram programState) (stateNextLine programState) (Just [-1]) newOutput (stateRelBase programState)
                 computer1 = Computer (computerNumber computer) (fromJust $ runProgramWithState newProgramState False) (computerMessageQueue computer) in
            (computer1, ComputerResult True sendPackets_)
    else
        let programState = computerProgramState computer
            newMessageQueue = drop 2 messageQueue
            newProgramState = createProgramState2 (stateProgram programState) (stateNextLine programState) (Just $ take 2 messageQueue) newOutput (stateRelBase programState)
            computer1 = Computer (computerNumber computer) (fromJust $ runProgramWithState newProgramState False) newMessageQueue in
        (computer1, ComputerResult False sendPackets_)

sendPackets :: ([Computer],  [Packet], Maybe Packet) -> ([Computer], [Packet], Maybe Packet)
sendPackets (computers, packets, nat) = 
    case packets of
        [] -> (computers, packets, nat)
        x:xs -> if isJust nat && packetDst x == 255
                    then (computers, xs, nat)
                else
                    let newComputers = map (\c -> if packetDst x == computerNumber c then Computer (packetDst x) (computerProgramState c) (computerMessageQueue c ++ [packetX x, packetY x]) else c) computers in
                    sendPackets (newComputers, xs, nat)

updatePrevNats :: [Packet] -> Packet -> [Packet]
updatePrevNats prevNats nat
    | null prevNats = [nat]
    | otherwise = [last prevNats, nat]

calculateNetwork :: ([Computer], Result, Maybe Packet, [Packet]) -> ([Computer], Result, Maybe Packet, [Packet])
calculateNetwork (computers, result, nat, prevNats) = 
    let temp = [calculateComputer c | c <- computers]
        computers1 = map fst temp
        results1 = map snd temp
        -- Send packets
        overallIdle = all resultIsIdle results1
        overallOutputQueue = concatMap outputQueue results1
        (computers2, _, _) = until (\(_, b, _) -> null b) sendPackets (computers1, overallOutputQueue, nat)
        isIdle = isJust nat && overallIdle && null overallOutputQueue
        (computers3, _, _) = if isIdle then sendPackets (computers2, [Packet 0 (packetX $ fromJust nat) (packetY $ fromJust nat)], nat) else (computers2, [], nat)
        newPrevNats = if isIdle && isJust nat then updatePrevNats prevNats (fromJust nat) else prevNats
        answer2_ = if isIdle && length newPrevNats == 2 && packetY (head newPrevNats) == packetY (last newPrevNats) then Just (packetY (last newPrevNats)) else Nothing
        packetIndices = findIndices (\x -> packetDst x == 255) overallOutputQueue -- Assmuded that there is only one packet to 255 in queue. Otherwise the assertion will be triggered.
        _ = assert(length packetIndices == 1) Nothing  -- assert cannot be before if-then-else?
        packet = if length packetIndices == 1 then Just (overallOutputQueue !! head packetIndices) else Nothing
        newNat = if isJust packet then packet else nat
        packetY_ = if isNothing packet then Nothing else Just $ packetY (fromJust packet)
        answer1_ = if isJust (answer1 result) then answer1 result else packetY_ in
    (computers3, Result answer1_ answer2_, newNat, newPrevNats)

main :: IO()
main = do
    program <- readProgram "Day23_input.txt"

    let computers = [Computer x (fromJust $ runProgram2 program 0 True (Just [x]) Nothing) [] | x <- [0..49]]
        result = (\(_, b, _, _) -> b) $ until checkResult calculateNetwork (computers, Result Nothing Nothing, Nothing, [])

    putStrLn "Question 1: What is the Y value of the first packet sent to address 255?"
    print $ fromJust (answer1 result)

    putStrLn "Question 2: What is the first Y value delivered by the NAT to the computer at address 0 twice in a row?"
    print $ fromJust (answer2 result)

-- Answer 1: 22659
-- Answer 2: 17429
