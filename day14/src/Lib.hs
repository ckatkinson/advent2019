module Lib
    ( fourteen
    ) where

import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List.Split


data Ingredient = 
  Ingredient 
  { name   :: String
  , amount :: Int } deriving (Show, Eq, Ord)

type Substance = String

type Recipe     = Map Ingredient [Ingredient]

type Supplies   = Map String Int

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

getRecipeFor :: Substance -> Recipe -> Recipe
getRecipeFor sub = M.filterWithKey (\k _ -> (name k == sub))


addSupplies :: Supplies -> Ingredient -> Supplies
addSupplies sup ingred = M.insertWith (+) (name ingred) (amount ingred) sup

useSupplies :: Supplies -> Ingredient -> Maybe Supplies
useSupplies sup (Ingredient n amt) =
  if n `M.member` sup && amt <= amtStored
    then Just (addSupplies sup (Ingredient n (-1 * amt)))
    else Nothing
  where amtStored = fromJust $ sup !? n


-- This gives answers that are too high. The reason is that it is not taking
-- into account "Wasted" product. I guess we need like a bank to draw from?
oreToMakeIng :: Recipe -> Supplies -> Ingredient -> (Int, Supplies)
oreToMakeIng _ s (Ingredient "ORE" n) = (n, s)
oreToMakeIng rec sup ing@(Ingredient element num)
  | isNothing (useSupplies sup ing) = oreToMakeIng rec (addSupplies sup ing) ing
  | otherwise  = 
      (sum $ map (\ (Ingredient el nel) -> fst $ oreToMakeIng rec (fromJust $ useSupplies sup ing) (Ingredient el (numRecipes num * nel)))
        toMake, (fromJust $ useSupplies sup ing))
  where elementRec = getRecipeFor element rec
        numRecipes num = ((num - 1) `quot` amt) + 1
        amt = amount $ head $ M.keys elementRec
        toMake = head $ M.elems elementRec



fourteen :: IO ()
fourteen = do recipe <- getInput "./testinput"
              print $ oreToMakeIng recipe M.empty (Ingredient "FUEL" 1)
