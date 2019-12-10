-- {-# OPTIONS_GHC -Wall #-}
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
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as S

getInput :: FilePath -> IO (Seq Int)
getInput path = do contents <- readFile path
                   return (S.fromList $ map read $ splitOneOf ", " contents)

data OpCode  = ADD | MUL | IN | OUT | JMT | JMF | LTN | EQU | HLT | AJR deriving (Show, Eq)
data ParMode = POS | IMM | REL deriving (Show, Eq)
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
                 , relBase :: Int
                 } deriving Show

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
               9  -> AJR -- adjust relative base
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
                 AJR -> 1
                 HLT -> 0

parMode :: Int -> ParMode
parMode 0 = POS
parMode 1 = IMM
parMode 2 = REL
parMode _ = error "Not a valid parameter mode"

parModes :: OpInst -> [ParMode]
parModes ins = take (numParams $ opCode ins) $
               map (parMode . digitToInt)  
                   (drop 2 $ reverse (show ins)) ++ replicate 3 POS

getArg :: Machine -> ParArg -> Arg
getArg mach (arg, par) = case par of
                          POS -> fromMaybe 0 (S.lookup arg mem)
                          IMM -> arg
                          REL -> fromMaybe 0 (S.lookup (arg + relBase mach) mem)
  where mem = memory mach

memLookup :: Memory -> Pointer -> Int
memLookup mem ptr = fromMaybe 0 (S.lookup ptr mem)

memWrite :: Memory -> Int -> Int -> Memory
memWrite mem val loc
  | loc > memsize = S.adjust' ( const val ) loc (mem >< zeros)
  | otherwise     = S.adjust' ( const val ) loc mem
    where zeros   = S.fromList (replicate (loc - memsize + 2) 0)
          memsize = S.length mem

execute :: Machine -> Machine
execute mach
  | currentoc == HLT = mach
  | currentoc == ADD = execute (Machine (memWrite mem (head args + (args !! 1))
                                                  storeAt)
                                                  shift
                                                  buff
                                                  rel)
  | currentoc == MUL = execute (Machine (memWrite mem (head args * (args !! 1))
                                                  storeAt)
                                                  shift
                                                  buff
                                                  rel)
  | currentoc == IN  =  if snd (head currentPArgs) == REL
                          then execute (Machine (memWrite mem (head buff)
                                        (rel + fst (head currentPArgs)))
                                         shift
                                         (tail buff) rel)
                          else if snd (head currentPArgs) == POS
                                 then execute (Machine (memWrite mem (head buff)
                                         storeAt)
                                         shift
                                         (tail buff) rel)
                                 else execute (Machine (memWrite mem (head buff)
                                        (ptr + numArgs))
                                        shift
                                        (tail buff) rel)
  | currentoc == OUT = execute (Machine mem shift (buff ++ pure (head args)) rel)
  | currentoc == JMT = if head args /= 0
                         then execute (Machine mem (last args) buff rel)
                         else execute (Machine mem shift buff rel)
  | currentoc == JMF = if head args == 0
                         then execute (Machine mem (last args) buff rel)
                         else execute (Machine mem shift buff rel)
  | currentoc == LTN = if head args < args !! 1
                         then execute (Machine (memWrite mem 1 storeAt) 
                                         shift buff rel)
                         else execute (Machine (memWrite mem 0 storeAt) 
                                         shift buff rel)
  | currentoc == EQU = if head args == args !! 1
                         then execute (Machine (memWrite mem 1 storeAt) 
                                         shift buff rel)
                         else execute (Machine (memWrite mem 0 storeAt) 
                                         shift buff rel)
  | currentoc == AJR = execute (Machine mem shift buff (rel + head args))
  | otherwise        = error "Syntax error or something else horrible"
  where mem           = memory mach
        ptr           = pointer mach
        buff          = buffer mach
        rel           = relBase mach
        ptrStar       = memLookup mem ptr
        currentoc     = opCode ptrStar
        numArgs       = numParams currentoc
        currentParams = parModes ptrStar
        currentPArgs  = zip [ memLookup mem (ptr + d) | d <- [1 .. numArgs] ] currentParams
        args          = map (getArg mach) currentPArgs
        shift         = movePointer currentoc ptr
        storeAt       = memLookup mem (ptr + numArgs)

-- t1 = Machine (S.fromList [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99])
             -- 0 [] 0
t1 = Machine (S.fromList [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99])
             0 [] 0

-- this works
t2 = Machine (S.fromList [1102,34915192,34915192,7,4,7,99,0]) 0 [] 0

--this works
t3 = Machine (S.fromList [104,1125899906842624,99]) 0 [] 0

--this works
t4 = Machine (S.fromList [204, 5, 99, 5555555]) 0 [] (-2)

t5 = Machine (S.fromList [1001, 100, 1, 100, 4, 100, 99]) 0 [] (-2)

-- 203 is failing
t6 = Machine (S.fromList [203, 4, 4, 5, 99, 5555555]) 0 [666] 1

--
t7 = Machine (S.fromList [203, 1, 99]) 0 [666] 2
