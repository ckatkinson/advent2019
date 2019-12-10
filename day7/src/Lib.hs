-- {-# OPTIONS_GHC -Wall #-}
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
initializeMachines :: Memory -> [Phase] -> Seq Machine
initializeMachines mem phs = S.fromList $ map (\ p -> Machine mem 0 (pure p) 0) phs

passBuffer :: Machine -> Machine -> Machine
passBuffer xMach yMach = Machine mem ptr buff rel
  where mem = memory yMach
        ptr = pointer yMach
        buff = buffer xMach
        rel = relBase yMach

currentMach :: AmpCycle -> Machine
currentMach ac = machines ac `S.index` aPointer ac

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
            | x == currM = stepCurr
            | x == nextM = newM
            | otherwise  = x

executeAC :: AmpCycle -> AmpCycle
executeAC ac
  -- | nextOpCode lastM == HLT = ac
  | nextOpCode (currentMach ac) == HLT = ac
  | otherwise               = executeAC (stepAmpCycle ac)
    where lastM = (machines ac) `S.index` (S.length (machines ac) - 1)
          

seven :: IO ()
seven = do mem <- getInput "./input"
           --print $ answer1 mem
           let ac = AmpCycle (initializeMachines mem [5,6,7,8,9]) 0
           --print $ fmap buffer $ machines $ stepAmpCycle ac
           print $ fmap buffer $ machines $ executeAC ac
