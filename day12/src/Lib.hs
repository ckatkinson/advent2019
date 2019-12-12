module Lib
    ( twelve
    ) where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M

input :: [Position]
input =  [(7 , 10, 17),
          (-2, 7 , 0),
          (12, 5 , 12),
          (5 , -8, 6)]

testinput =  [(-1, 0, 2),
              (2, -10, -7),
              (4, -8, 8),
              (3, 5, -1)]

initSystem :: System
initSystem = map (\ p -> Moon p (0,0,0)) input

testinitSystem :: System
testinitSystem = map (\ p -> Moon p (0,0,0)) testinput

m1 = head testinitSystem
m2 = testinitSystem !! 1
m3 = testinitSystem !! 2
m4 = testinitSystem !! 3


type Position = (Int, Int, Int)
type Velocity = (Int, Int, Int)
data Moon     = Moon { position :: Position
                     , velocity :: Velocity } deriving (Show, Eq, Ord)
type System   = [Moon]

-- Utility

addV :: Position -> Position -> Position
addV (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2) 

xcoord :: (Int, Int, Int) -> Int
xcoord (x, _, _) = x

ycoord :: (Int, Int, Int) -> Int
ycoord (_, y, _) = y

zcoord :: (Int, Int, Int) -> Int
zcoord (_, _, z) = z

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
  where newVelocity  = velocity moon1 `addV` dVel
        (x1, y1, z1) = position moon1
        (x2, y2, z2) = position moon2
        dVel         = (compGravChange x1 x2, compGravChange y1 y2, compGravChange z1 z2)


applyGravityMoon :: System -> Moon -> Velocity
applyGravityMoon sys moon = foldl addV (velocity moon) (map (velGravChange moon) sys)

stepMoon :: System -> Moon -> Moon
stepMoon sys moon = Moon newPos newVel
  where newVel = applyGravityMoon sys moon
        vel    = velocity moon
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

-- Maybe faster? Just checking energy each time. Should bue quite a bit faster
-- to check energy membership. However, we have to compute the energy each
-- time...
stepsToRepeat :: System -> Set System -> Set Energy -> Int
stepsToRepeat sys seen enSeen
  | S.notMember en enSeen = stepsToRepeat (stepSystem sys) newSeen newEnSeen
  | S.member en enSeen    = if S.notMember sys seen
                              then stepsToRepeat (stepSystem sys) newSeen newEnSeen
                              else S.size seen
  where newSeen   = S.insert sys seen
        newEnSeen = S.insert en enSeen
        en        = energy sys


-- This'll probably never terminate. Need to be smarter.
answer2 :: System -> Int
answer2 sys = stepsToRepeat sys S.empty S.empty

-- Maybe just look at each Moon individually to find its period?

type MoonNo = Int
type Step = Int


-- This isn't going to work...
-- moonPeriod :: System -> MoonNo -> Map (Position, Velocity) Int -> Step -> (Int, Int)
-- moonPeriod sys no rec step
  -- | M.notMember (pos, vel) rec = moonPeriod newSystem no (M.insert (pos, vel) step rec) (step + 1)
  -- | otherwise           = (step - startStep, startStep)
  -- where pos = position (sys !! no)
        -- vel = velocity (sys !! no)
        -- startStep = fromMaybe (-1) (rec !? (pos, vel))
        -- newSystem = stepSystem sys
        -- velAtStartStep = iterate stepSystem sys !! startStep
        -- velAtStep      = iterate stepSystem sys !! step


moonPeriod :: System -> MoonNo -> Map (Position, Velocity) Int -> Step -> (Int, Int)
moonPeriod sys no rec step
   | M.member (pos, vel) rec  = (step - startStep, startStep)
   | otherwise = moonPeriod newSystem no (M.insert (pos, vel) step rec) (step + 1)
   where pos = position (sys !! no)
         vel = velocity (sys !! no)
         startStep = fromMaybe (-1) (rec !? (pos, vel))
         newSystem = stepSystem sys
         velAtStartStep = iterate stepSystem sys !! startStep
         velAtStep      = iterate stepSystem sys !! step

  

twelve :: IO ()
twelve = do putStrLn "Part 1:"
            print $ answer1 initSystem 1000
            putStrLn "Part 2:"
            print $ answer2 initSystem
            -- print $ answerr initSystem
