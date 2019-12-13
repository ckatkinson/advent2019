module Lib
    ( thirteen
    ) where

import IntCode
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Point = (Int, Int)
data Tile = Empty | Wall | Block | Paddle | Ball deriving (Show, Eq)
type Board = Map Point Tile

intToTile :: Int -> Tile
intToTile n = case n of
                0 -> Empty
                1 -> Wall
                2 -> Block
                3 -> Paddle
                4 -> Ball
                _ -> error "This Int is not associated with a tile"

buildBoard :: Buffer -> Board
buildBoard (x:y:t:bs) = M.insert (x,y) (intToTile t) (buildBoard bs)
buildBoard _          = M.empty

answer1 :: Memory -> Int
answer1 mem = M.size $ M.filter (== Block) board
  where buff = buffer $ execute (Machine mem 0 [] 0)
        board = buildBoard buff
                 
 
thirteen :: IO ()
thirteen = do mem <- getInput "./input"
              print $ answer1 mem

-- The software draws tiles to the screen with output instructions: every three
-- output instructions specify the x position (distance from the left), y position
-- (distance from the top), and tile id. The tile id is interpreted as follows:
--
    -- 0 is an empty tile. No game object appears in this tile.  
    -- 1 is a wall tile.  Walls are indestructible barriers.  
    -- 2 is a block tile. Blocks can be broken by the ball.  
    -- 3 is a horizontal paddle tile. The paddle is indestructible.
    -- 4 is a ball tile. The ball moves diagonally and bounces off objects.
    --
    -- How many block tiles are on the screen when the game exits?

