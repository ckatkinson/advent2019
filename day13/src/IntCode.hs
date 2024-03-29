-- {-# OPTIONS_GHC -Wall #-}
module IntCode
    ( execute
    , getInput
    , Memory
    , Buffer
    , Pointer
    , Machine (..)
    , OpCode (..)
    , nextOpCode
    , step
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
                 , inBuffer  :: Buffer 
                 , outBuffer :: Buffer
                 , relBase :: Int
                 } deriving (Show, Eq)

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

nextOpCode :: Machine -> OpCode
nextOpCode mach = opCode (memLookup mem ptr)
  where mem = memory mach
        ptr = pointer mach

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
  | loc >= memsize = S.adjust' ( const val ) loc (mem >< zeros)
  | otherwise     = S.adjust' ( const val ) loc mem
    where zeros   = S.fromList (replicate (loc - memsize + 2) 0)
          memsize = S.length mem

execute :: Machine -> Machine
execute mach
  | nextOpCode mach == HLT = mach
  | otherwise              = execute $ step mach

step :: Machine -> Machine
step mach
  | currentoc == HLT = mach
  | currentoc == ADD = Machine (memWrite mem (head args + (args !! 1))
                                                  loc)
                                                  shift
                                                  inBuff outBuff
                                                  rel
  | currentoc == MUL = Machine (memWrite mem (head args * (args !! 1))
                                                  loc)
                                                  shift
                                                  inBuff outBuff
                                                  rel
  | currentoc == IN  = Machine (memWrite mem (head inBuff)
                                        loc)
                                        shift
                                        (tail inBuff) outBuff rel
  | currentoc == OUT = Machine mem shift inBuff (outBuff ++ pure (head args)) rel
  | currentoc == JMT = if head args /= 0
                         then  Machine mem (last args) inBuff outBuff rel
                         else  Machine mem shift inBuff outBuff rel
  | currentoc == JMF = if head args == 0
                         then  Machine mem (last args) inBuff outBuff rel
                         else  Machine mem shift inBuff outBuff rel
  | currentoc == LTN = if head args < args !! 1
                         then  Machine (memWrite mem 1 loc) 
                                         shift inBuff outBuff rel
                         else  Machine (memWrite mem 0 loc) 
                                         shift inBuff outBuff rel
  | currentoc == EQU = if head args == args !! 1
                         then  Machine (memWrite mem 1 loc) 
                                         shift inBuff outBuff rel
                         else  Machine (memWrite mem 0 loc) 
                                         shift inBuff outBuff rel
  | currentoc == AJR =  Machine mem shift inBuff outBuff (rel + head args)
  | otherwise        = error "Syntax error or something else horrible"
  where mem           = memory mach
        ptr           = pointer mach
        inBuff        = inBuffer mach
        outBuff        = outBuffer mach
        rel           = relBase mach
        ptrStar       = memLookup mem ptr
        currentoc     = opCode ptrStar
        numArgs       = numParams currentoc
        currentParams = parModes ptrStar
        currentPArgs  = zip [ memLookup mem (ptr + d) | d <- [1 .. numArgs] ] currentParams
        args          = map (getArg mach) currentPArgs
        shift         = movePointer currentoc ptr
        loc = case snd (last currentPArgs) of
                  REL -> rel + fst (last currentPArgs)
                  POS -> fst (last currentPArgs)
                  IMM -> fst (last currentPArgs)
