module Lib
    ( thirteen
    ) where

import IntCode
import Data.List.Split (chunksOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S

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

charTile :: Tile -> Char
charTile t = case t of
                 Empty  -> '·'
                 Wall   -> '▓'
                 Block  -> '▒'
                 Paddle -> '─'
                 Ball   -> '○' 

buildBoard :: Buffer -> Board
-- buildBoard [] = M.empty
buildBoard (y:x:t:bs)  
  | y == -1   = buildBoard bs
  | otherwise = M.insert (x,y) (intToTile t) (buildBoard bs)
buildBoard _ = M.empty

scoreBoard :: Buffer -> Int
scoreBoard [] = 0
scoreBoard (y:x:score:bs)
  | y == -1   = score
  | otherwise = scoreBoard bs


boardWidth :: Board -> Int
boardWidth board = maximum [ snd p | p <- M.keys board ] + 1

displayBoard :: Board -> String
displayBoard board = unlines $
                     map (map (charTile . snd)) $
                     chunksOf (boardWidth board) $
                     M.toAscList $ 
                     fixBoard board

-- A hack: for some reason, after stepping the machine, my board is missing the
-- first few tiles. Let's try a hack!

fixBoard :: Board -> Board
fixBoard = M.union (M.fromList [((0,0), Wall), ((0,1), Wall), ((0,2), Wall)])

                  

answer1 :: Memory -> Int
answer1 mem = M.size $ M.filter (== Block) board
  where buff = buffer $ execute (Machine mem 1 [] 0)
        board = buildBoard buff
                 

inputToBuffer :: Machine -> Buffer -> Machine
inputToBuffer (Machine m p _ r) b = Machine m p b r

flushBuffer :: Machine -> Machine
flushBuffer (Machine m p _ r) = Machine m p [] r
 
disp = putStrLn . displayBoard . buildBoard . buffer

thirteen :: IO ()
thirteen = do mem <- getInput "./input"
              let machFreePlay  = Machine (S.update 0 2 mem) 0 [1] 0
              let nextmach = execute machFreePlay 
              let nnextmach = execute $ inputToBuffer nextmach [1]
              disp nextmach
              disp nnextmach


-- The game didn't run because you didn't put in any quarters.  Unfortunately,
-- you did not bring any quarters. Memory address 0 represents the number of
-- quarters that have been inserted; set it to 2 to play for free.

-- The arcade cabinet has a joystick that can move left and right.  The software
-- reads the position of the joystick with input instructions:

-- If the joystick is in the neutral position, provide 0.  If the joystick is
-- tilted to the left, provide -1.  If the joystick is tilted to the right,
-- provide 1.

-- The arcade cabinet also has a segment display capable of showing a single
-- number that represents the player's current score. When three output
-- instructions specify X=-1, Y=0, the third output instruction is not a tile;
-- the value instead specifies the new score to show in the segment display. For
-- example, a sequence of output values like -1,0,12345 would show 12345 as the
-- player's current score.

-- Beat the game by breaking all the blocks. What is your score after the last
-- block is broken?
