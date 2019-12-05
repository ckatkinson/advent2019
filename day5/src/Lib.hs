{-# OPTIONS_GHC -Wall #-}
module Lib
    ( five
    ) where

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

readInt :: IO Int
readInt = read <$> getLine

execute :: Memory -> Pointer -> IO Memory
execute mem ptr
  | currentoc == HLT = return mem
  | currentoc == ADD = execute (S.adjust'
                                  (const $ head args + (args !! 1))
                                  storeAt
                                  mem)
                                  shift
  | currentoc == MUL = execute (S.adjust'
                                  (const $ head args * (args !! 1))
                                  storeAt
                                  mem)
                                  shift
  | currentoc == IN  = do x <- readInt
                          print args
                          execute (S.adjust' (const x)
                                             (memLookup mem (ptr + 1))
                                             mem)
                                  shift
  | currentoc == OUT = do print $ head args
                          execute mem shift
  | currentoc == JMT = if head args /= 0
                         then execute mem (last args)
                         else execute mem shift 
  | currentoc == JMF = if head args == 0
                         then execute mem (last args)
                         else execute mem shift 
  | currentoc == LTN = if head args < args !! 1
                         then execute (S.adjust' (const 1) storeAt mem) shift
                         else execute (S.adjust' (const 0) storeAt mem) shift
  | currentoc == EQU = if head args == args !! 1
                         then execute (S.adjust' (const 1) storeAt mem) shift
                         else execute (S.adjust' (const 0) storeAt mem) shift
  | otherwise        = error "Syntax error or something"
  where ptrStar       = memLookup mem ptr
        currentoc     = opCode ptrStar
        numArgs       = numParams currentoc
        currentParams = parModes ptrStar
        currentPArgs  = zip [ memLookup mem (ptr + d) | d <- [1 .. numArgs] ] currentParams
        args          = map (getArg mem) currentPArgs
        shift         = movePointer currentoc ptr
        storeAt       = memLookup mem (ptr + 3)

five :: IO Memory
five = do mem <- getInput "./input"
          execute mem 0

