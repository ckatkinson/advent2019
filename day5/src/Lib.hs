{-# OPTIONS_GHC -Wall #-}

module Lib
    ( five
    ) where

-- import Data.Map.Strict (Map, (!?))
-- import qualified Data.Map.Strict as M
import Data.List.Split (splitOneOf)
-- import Data.Maybe
-- import Data.Set (Set)
-- import qualified Data.Set as S

type Wire = [String]
                   
getLines :: FilePath -> IO [Wire]
getLines pth = do contents <- readFile pth
                  return $ map (splitOneOf ", ") $ lines contents


five :: IO ()
five = do xs <- getLines "./input"
          print xs
