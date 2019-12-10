-- {-# OPTIONS_GHC -Wall #-}
module Lib
    ( nine
    ) where

import IntCode
import qualified Data.Sequence as S


nine :: IO ()
nine = do xs <- getInput "./input"
          putStrLn "Part 1:"
          print $ head (buffer $ execute (Machine xs 0 [1] 0))
          putStrLn "Part 2:"
          print $ head (buffer $ execute (Machine xs 0 [2] 0))
           

