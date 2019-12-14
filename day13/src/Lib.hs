module Lib
    ( thirteen
    ) where

import IntCode
import Data.List.Split (chunksOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
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
                     M.toAscList
                     board
                  

answer1 :: Memory -> Int
answer1 mem = M.size $ M.filter (== Block) board
  where buff = outBuffer $ execute (Machine mem 0 [] [] 0)
        board = buildBoard buff

findBall :: Board -> Point
findBall b = head $ M.keys $ M.filter (==Ball) b

findPaddle :: Board -> Point
findPaddle b = head $ M.keys $ M.filter (==Paddle) b
                 

inputBuffer :: Buffer -> Machine -> Machine
inputBuffer b (Machine m p _ ob r) = Machine m p b ob r
 
display = putStrLn . displayBoard . buildBoard . outBuffer
sc = print . scoreBoard . outBuffer

flushOutput :: Machine -> Machine
flushOutput (Machine m p ib ob r) = Machine m p ib [] r

runGame :: Machine -> Board -> IO ()
runGame mach board  
  | nextOpCode mach == IN =  do putStrLn $ displayBoard newB
                                runGame newMach newB
  | nextOpCode mach == HLT = putStrLn $ displayBoard newB
                                -- print $ scoreBoard $ outBuffer mach
  | otherwise              = runGame (step mach) board
    where changes = buildBoard $ outBuffer mach
          newB = updateBoard changes board
          move = movePaddle board newB
          newMach = flushOutput $ step $ inputBuffer [move] mach

-- runGame :: Machine -> Board -> IO ()
-- runGame mach board = do let nextoc = nextOpCode mach
                        -- case nextoc of
                          -- IN  -> do let changes = buildBoard $ outBuffer mach
                                    -- let newB = updateBoard changes board
                                    -- putStrLn $ displayBoard newB
                                    -- -- print $ scoreBoard $ outBuffer mach
                                    -- -- print $ outBuffer mach
                                    -- let move = movePaddle board newB
                                    -- let newMach =  flushOutput $ step $ inputBuffer [move] mach
                                    -- runGame newMach newB
                          -- HLT  -> do let changes = buildBoard $ outBuffer mach
                                     -- let newB = updateBoard changes board
                                     -- putStrLn $ displayBoard newB
                                     -- -- print $ scoreBoard $ outBuffer mach
                          -- _   -> runGame (step mach) board

testThing :: Int -> IO ()
testThing n 
  | n == 0  = putStrLn "Zero!"
  | otherwise = do putStrLn "Subracting one!"
                   let m = n - 1
                   print m
                   testThing m


movePaddle :: Board -> Board -> Int
movePaddle currentBoard nextBoard
  | xPaddle < xBall = 1
  | xPaddle > xBall = -1
  | otherwise       = 0
  where xPaddle = fst $ findPaddle currentBoard
        xBall   = fst $ findBall nextBoard

-- runGame :: Machine -> IO ()
-- runGame mach = do let nextoc = nextOpCode mach
                  -- case nextoc of
                    -- IN  -> do display mach
                              -- runGame $ step mach
                    -- HLT -> display mach >> sc mach
                    -- _   -> runGame $ step mach

-- Welp. Reading reddit tells me that the output at later steps is ONLY the
-- squares that are updated. So just need to update the board. Tough to figure
-- this out...

updateBoard :: Board -> Board -> Board
updateBoard newBoard oldBoard = unionWithKey (\ _ c1 _ -> c1) newBoard oldBoard
  where unionWithKey f = merge preserveMissing preserveMissing (zipWithMatched f)


runWithInput :: Memory -> [Int] -> IO()
runWithInput memo inp = runGame (Machine (S.update 0 2 memo) 0 inp [] 0) initBoard
  where initBoard = buildBoard $ outBuffer $ execute (Machine memo 0 [] [] 0)

thirteen :: IO ()
thirteen = do mem <- getInput "./input"
              -- print $ answer1 mem
              -- print $ length $ outBuffer $ execute (Machine mem 0 [] [] 0)
              -- let machFreePlay  = Machine (S.update 0 2 mem) 0 ([1, 1, 1] ++ [0,0..]) [] 0
              -- print $ take 3 $ reverse $ outBuffer $ execute machFreePlay
              -- runGame $ machFreePlay
              runWithInput mem []




