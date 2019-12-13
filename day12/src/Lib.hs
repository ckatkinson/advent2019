{-# OPTIONS_GHC -Wall #-}
module Lib
    ( twelve
    ) where

input :: [Position]
input =  [(7 , 10, 17),
          (-2, 7 , 0),
          (12, 5 , 12),
          (5 , -8, 6)]

initSystem :: System
initSystem = map (\ p -> Moon p (0,0,0)) input

type Position = (Int, Int, Int)
type Velocity = (Int, Int, Int)
data Moon     = Moon { position :: Position
                     , velocity :: Velocity } deriving (Show, Eq, Ord)
type System   = [Moon]
type MoonComp  = (Int, Int)

-- Utility

addV :: Position -> Position -> Position
addV (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2) 

xcoord :: (Int, Int, Int) -> Int
xcoord (x, _, _) = x

ycoord :: (Int, Int, Int) -> Int
ycoord (_, y, _) = y

zcoord :: (Int, Int, Int) -> Int
zcoord (_, _, z) = z

moonX :: Moon -> MoonComp
moonX (Moon pos vel) = (xcoord pos, xcoord vel)

moonY :: Moon -> MoonComp
moonY (Moon pos vel) = (ycoord pos, ycoord vel)

moonZ :: Moon -> MoonComp
moonZ (Moon pos vel) = (zcoord pos, zcoord vel)

--

-- Dynamics

compGravChange :: Int -> Int -> Int
compGravChange x1 x2 
 | x1 > x2   = -1
 | x1 < x2   = 1
 | otherwise = 0


-- Careful: This is non-commutative. This is computing the new velocity for the
-- first argument.
velGravChange :: Moon -> Moon -> Velocity
velGravChange moon1 moon2 = dVel
  where (x1, y1, z1) = position moon1
        (x2, y2, z2) = position moon2
        dVel         = (compGravChange x1 x2, compGravChange y1 y2, compGravChange z1 z2)

applyGravityMoon :: System -> Moon -> Velocity
applyGravityMoon sys moon = foldl addV (velocity moon) (map (velGravChange moon) sys)

stepMoon :: System -> Moon -> Moon
stepMoon sys moon = Moon newPos newVel
  where newVel = applyGravityMoon sys moon
        newPos = position moon `addV` newVel

stepSystem :: System -> System
stepSystem sys = map (stepMoon sys) sys

--

-- Energy

type Energy = Int

potential :: Moon -> Energy
potential moon = abs (xcoord pos) + abs (ycoord pos) + abs (zcoord pos)
  where pos = position moon

kinetic :: Moon -> Energy
kinetic moon = abs (xcoord vel) + abs (ycoord vel) + abs (zcoord vel)
  where vel = velocity moon

total :: Moon -> Energy
total moon = potential moon * kinetic moon

energy :: System -> Energy
energy sys = sum $ map total sys

-- 

answer1 :: System -> Int -> Energy
answer1 sys n = energy $ iterate stepSystem sys !! n

-- Part 2

-- These function parallel above. It was too slow to work out the second part in
-- the above formulation. Much faster to do component by component. Can rewrite
-- the above in terms of these functions to reduce repetition, but I don't feel
-- like it.

stepMoonComp :: [MoonComp] -> MoonComp -> MoonComp
stepMoonComp sys m@(x,_) = (newP, newV)
  where newV = applyGravComp sys m
        newP   = x + newV

velGravChangeComp :: MoonComp -> MoonComp -> Int
velGravChangeComp (x1,_) (x2,_) = compGravChange x1 x2

applyGravComp :: [MoonComp] -> MoonComp -> Int
applyGravComp sys m@(_,v1) = foldl (+) v1 (map (velGravChangeComp m) sys)
      
stepSystemComp :: [MoonComp] -> [MoonComp]
stepSystemComp sys = map (stepMoonComp sys) sys

compPeriod :: [MoonComp] -> [MoonComp] -> Int -> Int
compPeriod mcs start step
  | start /= stepSystemComp mcs  = compPeriod (stepSystemComp mcs) start (step + 1)
  | otherwise           = step

answer2 :: System -> Int
answer2 sys = foldl lcm 1 [xp, yp, zp]
  where xp = compPeriod (map moonX sys) (map moonX sys) 1
        yp = compPeriod (map moonY sys) (map moonY sys) 1
        zp = compPeriod (map moonZ sys) (map moonZ sys) 1
  

twelve :: IO ()
twelve = do putStrLn "Part 1:"
            print $ answer1 initSystem 1000
            putStrLn "Part 2:"
            print $ answer2 initSystem
