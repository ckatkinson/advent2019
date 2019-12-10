-- {-# OPTIONS_GHC -Wall #-}
module Lib
    ( nine
    ) where

import IntCode
import qualified Data.Sequence as S



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

nine :: IO ()
nine = do xs <- getInput "./input"
          print $ execute (Machine xs 0 [1] 0)
          -- print $ execute t1
          -- print $ execute t2
          -- print $ execute t3
          -- print $ execute t4
          -- print $ buffer $ execute t6
           
           

