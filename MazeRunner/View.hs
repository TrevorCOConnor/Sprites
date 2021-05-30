module View where

import Field
import Movement
import Vision (distance)
import Data.Maybe

data View = View { viewRadius :: Float, viewField :: Field,  viewFocalPoint :: Position}

instance Show View where
    show = unlines . createViewDisplay


fieldEdge :: Field -> Position -> Maybe Char
fieldEdge field p@(x, y)
      | x == (-1) && y == (-1) = Just '╔'
      | x == (-1) && (-1) < y && y < fieldLength field = Just '║'
      | x == (-1) && y == fieldLength field = Just '╚'
      | (-1) < x && x < fieldWidth field && y == fieldLength field = Just '═'
      | x == fieldWidth field && y == fieldLength field = Just '╝' 
      | x == fieldWidth field && (-1) < y && y < fieldLength field = Just '║'
      | x == fieldWidth field && y == (-1) = Just '╗' 
      | (-1) < x && x < fieldWidth field && y == (-1) = Just '═'
      | otherwise = Nothing


safeContainsObject :: Field -> Position -> Bool
safeContainsObject = safeSquareCheck containsObject


safeIsObscure :: Field -> Position -> Bool
safeIsObscure = safeSquareCheck (not . sqrVisibility) 


safeSquareCheck :: (Square -> Bool) -> (Field -> Position -> Bool)
safeSquareCheck func = newFunc 
    where newFunc field p = isJust value && fromJust value
            where value = safeGetSquare p field >>= (Just . func)


wallSpot :: Field -> Position -> Maybe Char
wallSpot field p 
      | not (safeContainsObject field p) = Nothing
      | above && below && toLeft && toRight = Just '┼'
      | above && below && toLeft = Just '┤'
      | above && below && toRight = Just '├'
      | above && toLeft && toRight = Just '┴'
      | below && toLeft && toRight = Just '┬'
      | above && below = Just '│'
      | above && toLeft = Just '┘'
      | below && toLeft = Just '┐'
      | above && toRight = Just '└'
      | below && toRight = Just '┌' 
      | toLeft && toRight = Just '─'
      | above = Just '╵'
      | below = Just '╷'
      | toLeft = Just '╴'
      | toRight = Just '╶'
      | otherwise = Just '·'
    where above = safeContainsObject field (up p)
          below = safeContainsObject field (down p)
          toLeft  = safeContainsObject field (left p)
          toRight = safeContainsObject field (right p)


createSpot :: View -> Position -> Char
createSpot view p 
    | p == (viewFocalPoint view) = 'P'
    | distance p (viewFocalPoint view) > (viewRadius view) = ' '
    | isJust fieldedge = fromJust fieldedge
    | safeIsObscure field p = '*'
    | isJust wallspot = fromJust wallspot 
    | otherwise = ' '
    where fieldedge = fieldEdge field p
          wallspot = wallSpot field p
          field = viewField view

createViewDisplay :: View -> [String]
createViewDisplay view = map (map (createSpot view)) centeredRows
    where n = round $ viewRadius view
          rows = [ [(x,y) | y <- [0..(n-1)] ] | x <- [0..(n-1)]]
          centeredRows = centerDisplay (viewRadius view) (viewFocalPoint view) rows

centerDisplay :: Float -> Position -> [[Position]] -> [[Position]]
centerDisplay radius (x, y) = map (map (addPositions disp))
    where half = round (radius / 2) 
          disp = (x - half, y - half)
    
addPositions :: Position -> Position -> Position
addPositions (a, b) (x, y) = (a+x, b+y)
