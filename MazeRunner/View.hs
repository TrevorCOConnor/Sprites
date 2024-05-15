module View where

import Field
import Movement
import Vision (distance, edgeInLineOfSight)
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
                    && (not $ safeIsObscure field (up p))
          below = safeContainsObject field (down p)
                    && (not $ safeIsObscure field (down p))
          toLeft  = safeContainsObject field (left p)
                     && (not $ safeIsObscure field (left p))
          toRight = safeContainsObject field (right p)
                     && (not $ safeIsObscure field (right p))

createSpot :: View -> Position -> Char
createSpot view p 
    | p == (viewFocalPoint view) = 'P'
    | distance p focalPoint > radius = ' '
    | isJust fieldedge = if edgeInLineOfSight (round radius) focalPoint p field 
                            then fromJust fieldedge
                            else '*'
    | safeIsObscure field p = '*'
    | isJust wallspot = fromJust wallspot 
    | safeIsEnd field p = 'E'
    | isJust $ sqrmark = fromJust sqrmark 
    | isJust $ safeGetSquare p field = ' '
    | otherwise = '*'
    where fieldedge = fieldEdge field p
          wallspot = wallSpot field p
          field = viewField view
          radius = viewRadius view
          focalPoint = viewFocalPoint view
          sqrmark = safeGetSquare p field >>= sqrMark

createViewDisplay :: View -> [String]
createViewDisplay view = map (map (createSpot view)) centeredRows
    where n = round $ viewRadius view
          rows = [ [(x,y) | x <- [0..(n-1)] ] | y <- [0..(n-1)]]
          centeredRows = centerDisplay (viewRadius view) (viewFocalPoint view) rows

centerDisplay :: Float -> Position -> [[Position]] -> [[Position]]
centerDisplay radius (x, y) = map (map (addPositions disp))
    where half = round (radius / 2) 
          disp = (x - half, y - half)
    
addPositions :: Position -> Position -> Position
addPositions (a, b) (x, y) = (a+x, b+y)


createWholeSpot :: Field -> Square -> Char 
createWholeSpot field sqr
  | not (sqrVisibility sqr) = '*'
  | isJust fieldedge = fromJust fieldedge
  | isJust wallspot = fromJust wallspot
  | sqrEnd sqr = 'E'
  | not (isVacant sqr) = head . show . sqrOccupant $ sqr 
  | isJust $ sqrMark sqr = fromJust $ sqrMark sqr
  | otherwise = ' '
    where fieldedge = fieldEdge field (sqrPosition sqr)
          wallspot = wallSpot field (sqrPosition sqr) 


wholeFieldView :: Field -> String
wholeFieldView = unlines . wholeFieldView'


wholeFieldView' :: Field -> [String]
wholeFieldView' field = map (map (createWholeSpot field)) rows
    where rows = fieldRows field
