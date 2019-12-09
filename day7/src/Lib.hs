-- {-# OPTIONS_GHC -Wall #-}
module Lib
    ( seven
    ) where

import IntCode 
import Data.List (permutations)

type Phase = Int

startAmplifier :: Memory -> Phase -> Buffer -> Buffer
startAmplifier mem ph buff = execute mem 0 (ph : buff)

compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id

sequenceAmplifiers :: Memory -> [Phase] -> Buffer
sequenceAmplifiers mem phs = compose (startAmplifier mem <$> reverse phs) [0]

answer1 :: Memory -> Int
answer1 mem = maximum $ concatMap (sequenceAmplifiers mem) (permutations [0..4])

data AmpCycle = AmpCycle 
                 { memA :: Memory , ptrA :: Pointer, bufA :: Buffer
                 , memB :: Memory , ptrB :: Pointer, bufB :: Buffer
                 , memC :: Memory , ptrC :: Pointer, bufC :: Buffer
                 , memD :: Memory , ptrD :: Pointer, bufD :: Buffer
                 , memE :: Memory , ptrE :: Pointer, bufE :: Buffer
                 }
                 
initAmpCycle :: Memory -> [Phase] -> AmpCycle
initAmpCycle mem phs = AmpCycle mem 0 [head phs]
                                mem 0 [phs !! 1]
                                mem 0 [phs !! 2]
                                mem 0 [phs !! 3]
                                mem 0 [phs !! 4]
                 

seven :: IO ()
seven = do mem <- getInput "./input"
           print $ answer1 mem
