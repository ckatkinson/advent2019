{-# OPTIONS_GHC -Wall #-}
module Lib
    ( seven
    ) where

import IntCode 
import Data.List (permutations)
import Data.Sequence (Seq)
import qualified Data.Sequence as S

type Phase = Int

startAmplifier :: Memory -> Phase -> Buffer -> Buffer
startAmplifier mem ph buff = buffer $ execute (Machine mem 0 (ph : buff) 0)

compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id

sequenceAmplifiers :: Memory -> [Phase] -> Buffer
sequenceAmplifiers mem phs = compose (startAmplifier mem <$> reverse phs) [0]

answer1 :: Memory -> Int
answer1 mem = maximum $ concatMap (sequenceAmplifiers mem) (permutations [0..4])



type AmpPointer = Int
data AmpCycle = AmpCycle { machines :: Seq Machine, aPointer :: AmpPointer } deriving Show

-- Using step, the idea is as follows. Start running aMachine (preseeding buffer
-- with phase). If nextOpCode is OUT, then step and pass the buffer to the next
-- machine.


-- Give memory and list of phases. Get list of Machines.
-- Also will give the input 0 to the first machine (so it's input buffer should
-- be [p, 0]
initializeMachines :: Memory -> [Phase] -> Seq Machine
initializeMachines mem phs = S.fromList $ map (\ p -> Machine mem 0 (pure p) 0) phs

addInput :: Int -> Seq Machine -> Seq Machine
addInput n sms = S.update 0 withInput sms
  where frstMach  = sms `S.index` 0
        mem       = memory frstMach
        buff      = buffer frstMach
        withInput = Machine mem 0 (buff ++ [n]) 0

passBuffer :: Machine -> Machine -> Machine
passBuffer xMach yMach = Machine mem ptr buff rel
  where mem = memory yMach
        ptr = pointer yMach
        buff = buffer yMach ++ buffer xMach
        rel = relBase yMach

currentMach :: AmpCycle -> Machine
currentMach ac = machines ac `S.index` aPointer ac

flushBuffer :: Machine -> Machine
flushBuffer (Machine mem ptr _ rel) = Machine mem ptr [] rel

stepAmpCycle :: AmpCycle -> AmpCycle
stepAmpCycle ac
  | nextOpCode currM == OUT = AmpCycle (fmap update ms) nextPtr
  | otherwise               = AmpCycle (S.adjust step ap ms) ap
    where ms    = machines ac
          ap    = aPointer ac
          currM = currentMach ac
          numMs = S.length ms
          nextPtr = (ap + 1) `mod` numMs                 
          stepCurr = step currM
          nextM = ms `S.index` nextPtr
          newM = passBuffer stepCurr nextM
          update :: Machine -> Machine
          update x 
            | x == currM = flushBuffer stepCurr
            | x == nextM = newM
            | otherwise  = x

executeAC :: AmpCycle -> AmpCycle
executeAC ac
  | nextOpCode (currentMach ac) == HLT = ac
  | otherwise                          = executeAC (stepAmpCycle ac)
          
ampCycleOut :: AmpCycle -> Int
ampCycleOut ac = head $ fmap buffer (machines $ executeAC ac) `S.index` 0

answer2 :: Memory -> Int
answer2 mem = maximum $ map (ampCycleOut . ac) (permutations [5..9])
  where ac perm = AmpCycle (addInput 0 $ initializeMachines mem perm) 0

seven :: IO ()
seven = do mem <- getInput "./input"
           putStrLn "Part 1:"
           print $ answer1 mem
           putStrLn "Part 2:"
           print $ answer2 mem
