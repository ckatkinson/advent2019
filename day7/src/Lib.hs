-- {-# OPTIONS_GHC -Wall #-}
module Lib
    ( seven
    ) where

import IntCode 
import Data.List (permutations)

type Phase = Int

startAmplifier :: Memory -> Phase -> Buffer -> Buffer
startAmplifier mem ph buff = buffer $ execute (Machine mem 0 (ph : buff))

compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id

sequenceAmplifiers :: Memory -> [Phase] -> Buffer
sequenceAmplifiers mem phs = compose (startAmplifier mem <$> reverse phs) [0]

answer1 :: Memory -> Int
answer1 mem = maximum $ concatMap (sequenceAmplifiers mem) (permutations [0..4])

                 
                 


seven :: IO ()
seven = do mem <- getInput "./input"
           print $ answer1 mem
