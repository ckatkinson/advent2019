-- {-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}
module IntCode
    ( execute
    , getInput
    , Memory
    , Buffer
    , Pointer
    , Machine (..)
    )
    where

import Data.Char (digitToInt)
import Data.Maybe
import Data.List.Split (splitOneOf)
import Data.Sequence (Seq)
import qualified Data.Sequence as S

getInput :: FilePath -> IO (Seq Int)
getInput path = do contents <- readFile path
                   return (S.fromList $ map read $ splitOneOf ", " contents)

data OpCode  = ADD | MUL | IN | OUT | JMT | JMF | LTN | EQU | HLT deriving (Show, Eq)
data ParMode = POS | IMM deriving (Show, Eq)
type Pointer = Int
type OpInst  = Int 
type ParArg  = (Int, ParMode)
type Arg     = Int
type Memory  = Seq Int
type Buffer  = [Int]

data Machine = Machine
                 { memory  :: Memory
                 , pointer :: Pointer
                 , buffer  :: Buffer 
                 }

movePointer :: OpCode -> Pointer -> Pointer
movePointer oc ptr = let shift = numParams oc + 1
                     in ptr + shift

opCode :: OpInst -> OpCode
opCode ins = case ins `mod` 100 of
               1  -> ADD
               2  -> MUL
               3  -> IN
               4  -> OUT
               5  -> JMT
               6  -> JMF
               7  -> LTN
               8  -> EQU
               99 -> HLT
               _  -> error "Not a valid opCode"

numParams :: OpCode -> Int
numParams oc = case oc of
                 ADD -> 3
                 MUL -> 3
                 IN  -> 1
                 OUT -> 1
                 JMT -> 2
                 JMF -> 2
                 LTN -> 3
                 EQU -> 3
                 HLT -> 0

parMode :: Int -> ParMode
parMode 0 = POS
parMode 1 = IMM
parMode _ = error "Not a valid parameter mode"

parModes :: OpInst -> [ParMode]
parModes ins = take (numParams $ opCode ins) $
               map (parMode . digitToInt)  
                   (drop 2 $ reverse (show ins)) ++ replicate 3 POS

getArg :: Memory -> ParArg -> Arg
getArg mem (arg, par) = case par of
                          POS -> S.index mem arg
                          IMM -> arg

memLookup :: Memory -> Pointer -> Int
memLookup mem ptr = fromMaybe 0 (S.lookup ptr mem)

execute :: Machine -> Machine
execute mach
  | currentoc == HLT = mach
  | currentoc == ADD = execute (Machine (S.adjust'
                                  (const $ head args + (args !! 1))
                                  storeAt
                                  mem)
                                  shift
                                  buff)
  | currentoc == MUL = execute (Machine (S.adjust'
                                  (const $ head args * (args !! 1))
                                  storeAt
                                  mem)
                                  shift
                                  buff)
  | currentoc == IN  = execute (Machine (S.adjust' (const (head buff))
                                          (memLookup mem (ptr + 1))
                                          mem)
                                  shift
                                  (tail buff))
  | currentoc == OUT = execute (Machine mem shift (buff ++ pure (head args)))
  | currentoc == JMT = if head args /= 0
                         then execute (Machine mem (last args) buff)
                         else execute (Machine mem shift buff)
  | currentoc == JMF = if head args == 0
                         then execute (Machine mem (last args) buff)
                         else execute (Machine mem shift buff)
  | currentoc == LTN = if head args < args !! 1
                         then execute (Machine (S.adjust' (const 1) storeAt mem) shift buff)
                         else execute (Machine (S.adjust' (const 0) storeAt mem) shift buff)
  | currentoc == EQU = if head args == args !! 1
                         then execute (Machine (S.adjust' (const 1) storeAt mem) shift buff)
                         else execute (Machine (S.adjust' (const 0) storeAt mem) shift buff)
  | otherwise        = error "Syntax error or something else horrible"
  where mem           = memory mach
        ptr           = pointer mach
        buff          = buffer mach
        ptrStar       = memLookup mem ptr
        currentoc     = opCode ptrStar
        numArgs       = numParams currentoc
        currentParams = parModes ptrStar
        currentPArgs  = zip [ memLookup mem (ptr + d) | d <- [1 .. numArgs] ] currentParams
        args          = map (getArg mem) currentPArgs
        shift         = movePointer currentoc ptr
        storeAt       = memLookup mem (ptr + 3)

data AmpCycle = AmpCycle 
                 { aMachine :: Machine
                 , bMachine :: Machine
                 , cMachine :: Machine
                 , dMachine :: Machine
                 , eMachine :: Machine
                 , ampOrder :: [AmpCycle -> Machine]
                 }



initOrder :: [AmpCycle -> Machine]
initOrder = cycle [aMachine, bMachine, cMachine, dMachine, eMachine]


-- Using recordwildcards: need for cyExecute
-- modifyMem :: (Mem -> Mem) -> State -> State
-- modifyMem f State {..} = State { mem = f mem, .. }

cyExecute :: AmpCycle -> AmpCycle
cyExecute ac
  | currentoc == HLT  = ac
  | currentoc == ADD  = cyExecute (Machine (S.adjust'
                                   (const $ head args + (args !! 1))
                                   storeAt
                                   mem)
                                   shift
                                   buff)
  | currentoc == MUL  = cyExecute (Machine (S.adjust'
                                   (const $ head args * (args !! 1))
                                   storeAt
                                   mem)
                                   shift
                                   buff)
  | currentoc == IN   = cyExecute (Machine (S.adjust' (const (head buff))
                                           (memLookup mem (ptr + 1))
                                           mem)
                                   shift
                                   (tail buff))
  | currentoc == OUT  = cyExecute (Machine mem shift (buff ++ pure (head args)))
  | currentoc == JMT  = if head args /= 0
                          then cyExecute (Machine mem (last args) buff)
                          else cyExecute (Machine mem shift buff)
  | currentoc == JMF  = if head args == 0
                          then cyExecute (Machine mem (last args) buff)
                          else cyExecute (Machine mem shift buff)
  | currentoc == LTN  = if head args < args !! 1
                          then cyExecute (Machine (S.adjust' (const 1) storeAt mem) shift buff)
                          else cyExecute (Machine (S.adjust' (const 0) storeAt mem) shift buff)
  | currentoc == EQU  = if head args == args !! 1
                          then cyExecute (Machine (S.adjust' (const 1) storeAt mem) shift buff)
                          else cyExecute (Machine (S.adjust' (const 0) storeAt mem) shift buff)
  | otherwise         = error "Syntax error or something else horrible"
  where currentAmp    = head ampOrder ac
        mem           = memory currentAmp
        ptr           = pointer currentAmp
        buff          = buffer currentAmp
        ptrStar       = memLookup mem ptr
        currentoc     = opCode ptrStar
        numArgs       = numParams currentoc
        currentParams = parModes ptrStar
        currentPArgs  = zip [ memLookup mem (ptr + d) | d <- [1 .. numArgs] ] currentParams
        args          = map (getArg mem) currentPArgs
        shift         = movePointer currentoc ptr
        storeAt       = memLookup mem (ptr + 3)
