{-# OPTIONS_GHC -Wall #-}
module Lib
    ( eleven
    ) where

import IntCode
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Map.Merge.Strict

data Heading  = Up | Rght | Down | Lft deriving (Show, Eq, Ord)
type Position = (Int, Int)
data Color    = White | Black deriving (Show, Eq)
data Robot    = Robot { heading  :: Heading
                      , position :: Position } deriving Show
data Hull     = Hull { visited :: [Position]
                     , colors  :: Map Position Color 
                     , painted :: Set Position } deriving Show

type Configuration = (Hull, Robot, Machine)

getRobot :: Configuration -> Robot
getRobot (_,r,_) = r 

getHull :: Configuration -> Hull
getHull (h,_,_) = h

getMachine :: Configuration -> Machine
getMachine (_,_,m) = m

observeColor :: Position -> Hull -> Color
observeColor pos hul = fromMaybe Black $
                  M.lookup pos (colors hul)

paintPosition :: Position -> Color -> Hull ->  Hull
paintPosition pos col (Hull v cols ptd) = 
    Hull v (M.insert pos col cols) (S.insert pos ptd)

intToColor :: Int -> Color
intToColor 0 = Black
intToColor 1 = White
intToColor _ = error "Invalid color code"

colorToInt :: Color -> Int
colorToInt Black = 0
colorToInt White = 1

turnRght :: Heading -> Heading
turnRght Up   = Rght
turnRght Rght = Down
turnRght Down = Lft
turnRght Lft  = Up

turnLft :: Heading -> Heading
turnLft Up   = Lft
turnLft Rght = Up
turnLft Down = Rght
turnLft Lft  = Down

type Vector = Position

vectorHeading :: Heading -> Vector
vectorHeading Up   = (0,1)
vectorHeading Down = (0,-1)
vectorHeading Lft  = (-1,0)
vectorHeading Rght = (1,0)

addV :: (Int, Int) -> (Int, Int) -> (Int, Int)
addV (x1,y1) (x2,y2) = (x1+x2, y1+y2)

intToTurn :: Int -> (Heading -> Heading)
intToTurn 0 = turnLft
intToTurn 1 = turnRght
intToTurn _ = error "Invalid turn code"

moveRobot :: Robot -> Heading -> Robot
moveRobot (Robot _ pos) newHd = Robot newHd newPos
  where newPos = pos `addV` vectorHeading newHd

stepRobot :: Configuration -> Configuration
stepRobot config@(_, _, Machine _ _ [] _) = config
stepRobot config@(_, _, Machine _ _ [_] _) = config
stepRobot (hul, rob@(Robot hd pos), Machine m p (col : mv : buff) r) =  
  (paintPosition pos newCol hul, moveRobot rob newHd, Machine m p (newObs:buff) r)
  where turn = intToTurn mv 
        newHd = turn hd
        newCol = intToColor col
        newPos = position $ moveRobot rob newHd
        newObs = colorToInt $ observeColor newPos hul 

stepMachine :: Configuration -> Configuration
stepMachine (r, h, mach) = (r, h, step mach)

runRobot :: Configuration -> Configuration
runRobot config
  | nextOpCode (getMachine config) == HLT = config
  | otherwise                             = runRobot $ stepMachine $ 
                                            stepRobot config
                                        
initRobot :: Robot
initRobot = Robot Up (0,0)

initHull :: Hull
initHull = Hull [(0,0)] M.empty S.empty

initConfig :: Memory -> Configuration
initConfig mem = (initHull, initRobot, Machine mem 0 [0] 0)

initConfig2 :: Memory -> Configuration
initConfig2 mem = (initHull, initRobot, Machine mem 0 [1] 0)

answer1 :: Memory -> Int
answer1 mem = S.size $ painted $ getHull $ runRobot $ initConfig mem

--

hullMaxX :: Hull -> Int 
hullMaxX hull = maximum $ map fst $ M.keys (colors hull)

hullMinX :: Hull -> Int 
hullMinX hull = minimum $ map fst $ M.keys (colors hull)

hullMaxY :: Hull -> Int 
hullMaxY hull = maximum $ map snd $ M.keys (colors hull)

hullMinY :: Hull -> Int 
hullMinY hull = minimum $ map snd $ M.keys (colors hull)

hullWidth :: Hull -> Int
hullWidth h = hullMaxX h - hullMinX h

hullHeight :: Hull -> Int
hullHeight h = hullMaxY h - hullMinY h


allBlack :: Hull -> Map Position Color
allBlack hull = M.fromList [ ( (x,y), Black) | x<-[hullMinX hull .. hullMaxX hull],
                                               y<-[hullMinY hull .. hullMaxY hull] ]
  


colorsToList :: Hull -> [Int]
colorsToList hul = nums
 where cols = colors hul
       full = unionWithKey (\ _ c1 _ -> c1) cols (allBlack hul)
       lst = M.toAscList full
       nums = map ( \ (_, c) -> colorToInt c ) lst
       unionWithKey f = merge preserveMissing preserveMissing (zipWithMatched f)

makePbm :: Hull -> IO ()
makePbm hull = writeFile "./message.pbm" pbm
  where pbm = "P1 " ++ show (hullHeight hull + 1) ++
              " " ++ show (hullWidth hull + 1) ++
              " " ++ unwords (map show im)
        im = colorsToList hull

answer2 :: Memory -> IO ()
answer2 mem = makePbm hul
  where hul = getHull $ runRobot $ initConfig2 mem

eleven :: IO ()
eleven = do mem <- getInput "./input"
            putStrLn "Part 1:"
            print $ answer1 mem
            putStrLn "Open message.pbm to see answer to part 2"
            answer2 mem
