-- {-# OPTIONS_GHC -Wall #-}
module Lib
    ( five
    ) where

import Data.Char (digitToInt)
import Data.List (reverse)
import Data.List.Split (splitOneOf)
import Data.Sequence (Seq)
import qualified Data.Sequence as S

getInput :: FilePath -> IO (Seq Int)
getInput path = do contents <- readFile path
                   return (S.fromList $ map read $ splitOneOf ", " contents)

data OpCode = ADD | MUL | IN | OUT deriving (Show, Eq)
data ParMode = POS | IMM deriving (Show, Eq)
type Pointer = Int
type OpInst = Int -- Think something like 01002, encoding opcode and parmode
type Arg = Int

movePointer :: OpCode -> Pointer -> Pointer
movePointer oc ptr = let shift = numParams oc + 1
                     in case oc of
                          ADD -> ptr + shift
                          MUL -> ptr + shift
                          IN  -> ptr + shift
                          OUT -> ptr + shift

opcode :: OpInst -> OpCode
opcode ins = case ins `mod` 10 of
               1 -> ADD
               2 -> MUL
               3 -> IN
               4 -> OUT

numParams :: OpCode -> Int
numParams oc = case oc of
                 ADD -> 3
                 MUL -> 3
                 IN  -> 1
                 OUT -> 1

parMode :: Int -> ParMode
parMode 0 = POS
parMode 1 = IMM

-- The list of parmodes is usually too long in the else case. Plan is to zip it
-- together with the list of arguments so that the extra 0's at the end get
-- dropped.
parModes :: OpInst -> [ParMode]
parModes ins = map (parMode . digitToInt)  
                   (drop 2 $ reverse (show ins)) ++ replicate 3 POS

-- -- -- -- -- -- 

execute :: Seq Int -> Int -> Seq Int
execute s pos
  | S.index s pos == 99 = s 
  | S.index s pos == 1  = execute (S.adjust'
                                  (const $ S.index s si1 + 
                                           S.index s si2)
                                  si3
                                  s)
                                  (pos + 4) -- add
  | S.index s pos == 2  = execute (S.adjust'
                                  (const $ S.index s si1 * 
                                           S.index s si2)
                                  si3
                                  s)
                                  (pos + 4) -- multiply
  | otherwise           = error "Operation is undefined"
  where si1 = S.index s (pos + 1)
        si2 = S.index s (pos + 2)
        si3 = S.index s (pos + 3)

-- before running the program, replace position 1 with the value 12 and replace
-- position 2 with the value 2

answer1 :: Seq Int -> Int
answer1 s = S.index (execute s' 0) 0
  where s' = S.update 2 2 $ S.update 1 12 s


-- part 2
-- determine what pair of inputs produces the output 19690720
--

output :: Seq Int -> Int -> Int -> Int
output s noun verb = S.index (execute s' 0) 0
  where s' = S.update 2 verb $ S.update 1 noun s

formatOutput :: [(Int,Int)] -> Int
formatOutput ls = 100 * fst (head ls) * snd (head ls)


five :: IO ()
five = putStrLn "Hi"

