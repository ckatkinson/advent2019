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
import Data.Sequence (Seq)
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
                          POS -> S.index mem arg
                          IMM -> arg
                          REL -> S.index mem (relBase mach)
  where mem = memory mach

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
                                  buff
                                  rel)
  | currentoc == MUL = execute (Machine (S.adjust'
                                  (const $ head args * (args !! 1))
                                  storeAt
                                  mem)
                                  shift
                                  buff
                                  rel)
  | currentoc == IN  = execute (Machine (S.adjust' (const (head buff))
                                          (memLookup mem (ptr + 1))
                                          mem)
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
                         then execute (Machine (S.adjust' (const 1) storeAt mem) 
                                         shift buff rel)
                         else execute (Machine (S.adjust' (const 0) storeAt mem) 
                                         shift buff rel)
  | currentoc == EQU = if head args == args !! 1
                         then execute (Machine (S.adjust' (const 1) storeAt mem) 
                                         shift buff rel)
                         else execute (Machine (S.adjust' (const 0) storeAt mem) 
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
        storeAt       = memLookup mem (ptr + 3)

