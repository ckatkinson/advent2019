module Lib
    ( fourteen
    ) where

import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M
import Data.List.Split


data Ingredient = 
  Ingredient 
  { name   :: String
  , amount :: Int } deriving (Show, Eq, Ord)

type Recipe     = Map Ingredient [Ingredient]

getInput :: FilePath -> IO Recipe
getInput path = do 
  contents <- readFile path
  return (M.fromList $ map mkRec (lines contents))
  where 
  mkRec line = (head $ readIngs $ recTarget line, readIngs $ ings line)
  ings s = takeWhile (/= "=>") $ map (filter (/= ',')) $ words s
  recTarget s = tail $ dropWhile (/= "=>") $ words s
  readIngs :: [String] -> [Ingredient]
  readIngs [] = []
  readIngs (amt:nm:rest) = Ingredient nm (read amt) : readIngs rest

oreToMakeIng :: Recipe -> Ingredient -> Int
oreToMakeIng _ (Ingredient "ORE" n) = n
oreToMakeIng rec (Ingredient element num) = undefined



fourteen :: IO ()
fourteen = do recipe <- getInput "./input"
              print $ oreToMakeIng recipe (Ingredient "ORE" 5)
