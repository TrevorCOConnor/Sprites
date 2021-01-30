module Shapes where

import Core

data Shape = Circle | Square | Triangle

type Start  = Position
type Size   = Int
type Point  = (Float, Float)
type Angle  = Float

d1 :: Position -> Position -> Int
d1 (a, b) (x, y) = max (abs (x-a)) (abs (y-b))

d2 :: Position -> Position -> Float
d2 p1 p2 = sqrt $ (x-a)^2 + (y-b)^2
    where (a, b) = floatPosition p1
          (x, y) = floatPosition p2

pointToDirection :: Position -> Position -> Direction
pointToDirection (a, b) (x, y) | (x - a) > 0 = East
                               | (x - a) < 0 = West
                               | (y - b) > 0 = North
                               | (y - b) < 0 = South
directionToAngles :: Direction -> Point
directionToAngles South = (pi/4,      (3*pi)/4)
directionToAngles East  = (-pi/4,     pi/4)
directionToAngles North = (-(3*pi)/4, -pi/4)
directionToAngles West  = ((3*pi)/4,  (5*pi)/4)

positionsEqual :: Position -> Position -> Bool
positionsEqual (a, b) (x, y) = a == x && b == y

subtractPoints :: Point -> Point -> Point
subtractPoints p1 p2 = addPoints p1 $ scalePoint (-1.0) p2

addPoints :: Point -> Point -> Point
addPoints (a, b) (x, y) = (a + x, b + y)

scalePoint :: Float -> Point -> Point
scalePoint a (x, y) = (a*x, a*y)

floatPosition :: Position -> Point
floatPosition (x, y) = (x', y')
    where x' = fromIntegral x :: Float
          y' = fromIntegral y :: Float

roundPoint :: Point -> Position
roundPoint (x, y) = (round x, round y)

line :: Position -> Position -> [Position]
line p1 p2 = map (roundPoint . lineEq) ts
    where ts = [0.0, 0.1 .. 1.0]
          lineEq t = addPoints p1' $ scalePoint t $ subtractPoints p2' p1'
          p1' = floatPosition p1
          p2' = floatPosition p2

angle :: Position -> Position -> Float
angle p1@(a, b) p2@(x, y) = acos $ x' / radius
    where radius = d2 p1 p2
          x' = fromIntegral (x-a) :: Float

polarPoint :: Start -> Radius -> Angle -> Position
polarPoint start r angle = roundPoint $ (x, y)
    where (a, b) = floatPosition start 
          radius = fromIntegral r :: Float
          x = (radius * cos angle) + a
          y = (radius * sin angle) + b

plotLine :: Position -> Board -> Position -> [Position]
plotLine p1 board p2 = plotPoints ls board
    where ls = filter (not . positionsEqual p1) $ line p1 p2

plotPoints :: [Position] -> Board -> [Position]
plotPoints []     _     = []
plotPoints (p:ps) board = 
    case o of
      Vacant          -> p: plotPoints ps board
      ContainObject   -> []
      ContainSprite _ -> [p]
    where o = occupant $ getSquare p board

createCone :: Start -> Direction -> Radius -> Board -> [Position]
createCone start dir r board = 
    concat $ map (plotLine start board) $ map (polarPoint start r) angles
    where (startAngle, endAngle) = directionToAngles dir
          angles = [startAngle, startAngle + (1/50) .. endAngle]

