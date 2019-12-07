-- {-# OPTIONS_GHC -Wall #-}
module Lib
    ( seven
    ) where

import IntCode 
import Data.List (permutations)
import qualified Data.Sequence as S

-- import Data.Function
-- import Data.List (maximumBy, intersect)
import Data.List.Split (splitOneOf)
-- import Data.Maybe
-- import Data.Set (Set)
-- import qualified Data.Set as S

type Phase = Int

                   
startAmplifier :: Memory -> Phase -> Buffer -> Buffer
startAmplifier mem ph buff = execute mem 0 (ph : buff)

-- sequenceAmplifiers' :: Memory -> [Phase] -> Buffer
-- sequenceAmplifiers' mem phs = startAmplifier mem (phs !! 4) $
                             -- startAmplifier mem (phs !! 3) $
                             -- startAmplifier mem (phs !! 2) $
                             -- startAmplifier mem (phs !! 1) $
                             -- startAmplifier mem (head phs) [0]

compose :: [a -> a] -> a -> a
compose = foldl (flip (.)) id

sequenceAmplifiers :: Memory -> [Phase] -> Buffer
sequenceAmplifiers mem phs = compose (startAmplifier mem <$> reverse phs) [0]


answer1 :: Memory -> Int
answer1 mem = maximum $ concatMap (sequenceAmplifiers mem) (permutations [0..4])

test :: Memory
test = S.fromList [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
  1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]

seven :: IO ()
seven = do mem <- getInput "./input"
           print $ answer1 mem
