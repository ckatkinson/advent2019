{-# OPTIONS_GHC -Wall #-}
module Lib
    ( two
    ) where

import Data.List.Split (splitOneOf)
import Data.Sequence (Seq)
import qualified Data.Sequence as S

getInput :: FilePath -> IO (Seq Int)
getInput path = do contents <- readFile path
                   return (S.fromList $ map read $ splitOneOf ", " contents)

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



two' :: IO ()
two' = do xs <- getInput "./input"
          putStrLn "Answer to part 1 is:"
          print $ answer1 xs
          putStrLn "Answer to part 2 is:"
          let ls =  [(noun, verb) | (noun, verb) <- (,) <$> [0..99] <*> [0..99],
                                  output xs noun verb == 19690720]
          print $ formatOutput ls

-- Below is just a personal exercise to check that I understand this.
two :: IO ()
two = putStrLn "Answer to part 1 is:" >>
      getInput "./input" >>= 
      print . answer1 >> 
      putStrLn "Answer to part 2 is:" >>
      getInput "./input" >>= 
      \xs -> print . formatOutput $ 
             [(noun, verb) | (noun, verb) <- (,) <$> [0..99] <*> [0..99],
                                 output xs noun verb == 19690720]

