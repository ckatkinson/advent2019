-- {-# OPTIONS_GHC -Wall #-}
module Lib
    ( five
    ) where

import Data.Char (digitToInt)
import Data.List (reverse)
import Data.Maybe
import Data.List.Split (splitOneOf)
import Data.Sequence (Seq)
import qualified Data.Sequence as S

getInput :: FilePath -> IO (Seq Int)
getInput path = do contents <- readFile path
                   return (S.fromList $ map read $ splitOneOf ", " contents)

data OpCode  = ADD | MUL | IN | OUT | HLT deriving (Show, Eq)
data ParMode = POS | IMM deriving (Show, Eq)
type Pointer = Int
type OpInst  = Int -- Think something like 01002, encoding opcode and parmode
type ParArg  = (Int, ParMode)
type Arg     = Int
type Memory  = Seq Int

movePointer :: OpCode -> Pointer -> Pointer
movePointer oc ptr = let shift = numParams oc + 1
                     in case oc of
                          ADD -> ptr + shift
                          MUL -> ptr + shift
                          IN  -> ptr + shift
                          OUT -> ptr + shift

opCode :: OpInst -> OpCode
opCode ins = case ins `mod` 100 of
               1  -> ADD
               2  -> MUL
               3  -> IN
               4  -> OUT
               99 -> HLT
               _ -> error "hmmm"

numParams :: OpCode -> Int
numParams oc = case oc of
                 ADD -> 3
                 MUL -> 3
                 IN  -> 1
                 OUT -> 1
                 HLT -> 0

parMode :: Int -> ParMode
parMode 0 = POS
parMode 1 = IMM

-- The list of parmodes is usually too long in the else case. Plan is to zip it
-- together with the list of arguments so that the extra 0's at the end get
-- dropped.
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

readInt :: IO Int
readInt = read <$> getLine

execute :: Memory -> Pointer -> IO Memory
execute mem ptr
  | currentoc == HLT = return mem
  | currentoc == ADD = execute (S.adjust'
                                  (const $ head args + (args !! 1))
                                  (fst (currentPArgs !! 2))
                                  mem)
                                  shift
  | currentoc == MUL = execute (S.adjust'
                                  (const $ head args * (args !! 1))
                                  (fst (currentPArgs !! 2))
                                  mem)
                                  shift
  | currentoc == IN  = do x <- readInt
                          print args
                          execute (S.adjust' (const x)
                                             (memLookup mem (ptr + 1))
                                             --(head args)
                                             mem)
                                  shift
  | currentoc == OUT = do print $ head args
                          execute mem shift
  where ptrStar       = memLookup mem ptr
        currentoc     = opCode ptrStar
        numArgs       = numParams currentoc
        currentParams = parModes ptrStar
        currentPArgs  = zip [ memLookup mem (ptr + d) | d <- [1 .. numArgs] ] currentParams
        args          = map (getArg mem) currentPArgs
        shift         = movePointer currentoc ptr

mem :: Memory
mem = S.fromList [11101,1,1,1,99]

mem1 :: Memory
mem1 = S.fromList [3,0,11101,1,1,1,99]

mem2 :: Memory
mem2 = S.fromList [4,2,99]




five :: IO Memory
five = do mem <- getInput "./input"
          execute mem 0

