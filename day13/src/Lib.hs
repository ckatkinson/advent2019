{-# OPTIONS_GHC -Wall #-}
module Lib
    ( thirteen
    ) where

import IntCode
import Data.List.Split (chunksOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Sequence as S
import Data.Map.Merge.Strict

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
buildBoard (y:x:t:bs)  
  | y == -1   = buildBoard bs
  | otherwise = M.insert (x,y) (intToTile t) (buildBoard bs)
buildBoard _ = M.empty

scoreBoard :: Buffer -> Int
scoreBoard [] = 0
scoreBoard ((-1):0:score:bs) = score
scoreBoard (y:x:z:bs) = scoreBoard bs


boardWidth :: Board -> Int
boardWidth board = maximum [ snd p | p <- M.keys board ] + 1

displayBoard :: Board -> String
displayBoard board = unlines $
                     map (map (charTile . snd)) $
                     chunksOf (boardWidth board) $
                     M.toAscList
                     board
                  

answer1 :: Memory -> Int
answer1 mem = M.size $ M.filter (== Block) board
  where buff = outBuffer $ execute (Machine mem 0 [] [] 0)
        board = buildBoard buff

findBall :: Board -> Maybe Point
findBall b  
 | null bs = Nothing
 | otherwise = Just $ head bs
 where bs = M.keys $ M.filter (==Ball) b

findPaddle :: Board -> Maybe Point
findPaddle b 
 | null bs = Nothing
 | otherwise = Just $ head bs
 where bs = M.keys $ M.filter (==Paddle) b
                 

inputBuffer :: Int -> Machine -> Machine
inputBuffer b (Machine m p b1 ob r) = Machine m p (b:b1) ob r
 
flushOutput :: Machine -> Machine
flushOutput (Machine m p ib _ r) = Machine m p ib [] r

updatePointer :: Machine -> Pointer -> Machine
updatePointer (Machine m _ ib ob r) p = Machine m p ib ob r

runGame :: Machine -> Board -> IO ()
runGame mach board  
  | nextOpCode mach == IN =  if score /= 0
                               then do print score
                                       runGame newMach newB
                               else runGame newMach newB

  | nextOpCode mach == HLT = do putStrLn "HLT"
                                runGame (updatePointer mach 0) board
  | otherwise              = runGame (step mach) board
    where changes = buildBoard $ outBuffer mach
          newB = updateBoard changes board
          move = fromMaybe 1 $ movePaddle newB newB
          newMach = flushOutput $ step $ inputBuffer move mach
          score = scoreBoard $ outBuffer mach


movePaddle :: Board -> Board -> Maybe Int
movePaddle board board1
  | xPaddle < xBall  = Just 1
  | xPaddle > xBall  = Just $ -1
  | xPaddle == xBall = Just 0
  | otherwise        = Nothing
  where xPaddle = snd <$> findPaddle board
        xBall   = snd <$> findBall board1


updateBoard :: Board -> Board -> Board
updateBoard = unionWithKey (\ _ c1 _ -> c1)
  where unionWithKey f = merge preserveMissing preserveMissing (zipWithMatched f)

thirteen :: IO ()
thirteen = do mem <- getInput "./input"
              putStrLn "Part 1:"
              print $ answer1 mem
              let initBoard = buildBoard $ outBuffer $ execute (Machine mem 0 [] [] 0)
              let machFreePlay  = Machine (S.update 0 2 mem) 0 [] [] 0
              runGame machFreePlay initBoard -- Examine the output of this. Eventually it stops outputting (BUT KEEPS RUNNING!) That's the answer to part 2


