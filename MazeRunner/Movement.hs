module Movement where

import Field
import Data.Maybe

type Position = (Int, Int)

move :: Position -> Position -> Field -> (Position, Field)
move p1 p2 field = if validPos p2 field
                      then (p2, newField) 
                      else (p1, field)
    where occ = sqrOccupant $ getSquare p1 field
          newField = modifySquare (markTraversed . placeOccupant occ) p2 . modifySquare makeVacant p1 $ field

moveUp :: Position -> Field -> (Position, Field)
moveUp p = move p (up p)

moveDown :: Position -> Field -> (Position, Field)
moveDown p = move p (down p)

moveLeft :: Position -> Field -> (Position, Field)
moveLeft p = move p (left p) 
moveRight :: Position -> Field -> (Position, Field)
moveRight p = move p (right p)

getSquare :: Position -> Field -> Square
getSquare (x, y) field = r !! (x)
    where rs = fieldRows field
          r  = rs !! (y)

safeGetSquare :: Position -> Field -> Maybe Square
safeGetSquare p field = if inField p field
                         then Just $ getSquare p field
                         else Nothing

modifySquare :: (Square -> Square) -> Position -> Field -> Field
modifySquare f (x, y) field = field { fieldRows = rs' }
    where rs' = bRows ++ r':aRows
          (bRows, r:aRows) = splitAt (y) (fieldRows field)
          (bCols, c:aCols) = splitAt (x) r
          c' = f c
          r' = bCols ++ c':aCols

place :: Occupant -> Square -> Square
place occ sqr = sqr { sqrOccupant = occ }

placeObj :: Square -> Square
placeObj = place ContainsObject 

placeObjOnField :: Position -> Field -> Field
placeObjOnField = modifySquare placeObj

safePlaceObjOnField :: Position -> Field -> Field
safePlaceObjOnField pos field = if isJust vacancy && fromJust vacancy
                                   then placeObjOnField pos field
                                   else field
    where vacancy = (safeGetSquare pos field) >>= (Just . isVacant)

placeObjListOnField :: Field -> [Position] -> Field
placeObjListOnField = foldr placeObjOnField 

placePlayer :: Square -> Square
placePlayer = place ContainsPlayer

placePlayerOnField :: Position -> Field -> Field
placePlayerOnField = modifySquare $ markTraversed . placePlayer

placeEnd :: Square -> Square
placeEnd sqr = sqr {sqrEnd = True}

placeEndOnField :: Position -> Field -> Field
placeEndOnField = modifySquare placeEnd

markTraversed :: Square -> Square
markTraversed sqr = sqr { sqrTraversed = True }

markSquare :: Char -> Square -> Square
markSquare c sqr = sqr { sqrMark = Just c }

makeVacant :: Square -> Square
makeVacant sqr = sqr { sqrOccupant = Vacant }

makePositionVacant :: Position -> Field -> Field
makePositionVacant = modifySquare makeVacant

isVacant :: Square -> Bool
isVacant = (Vacant ==) . sqrOccupant

containsObject :: Square -> Bool
containsObject = (ContainsObject ==) . sqrOccupant

placeOccupant :: Occupant -> Square -> Square
placeOccupant o sqr = sqr { sqrOccupant = o }

validPos :: Position -> Field -> Bool
validPos (x, y) field = (inField (x, y) field) && isVacant sqr
    where sqr = getSquare (x,y) field

inField :: Position -> Field -> Bool
inField (x, y) field = xvalid && yvalid 
    where xvalid = 0 <= x && x < fieldWidth field
          yvalid = 0 <= y && y < fieldLength field
          sqr = getSquare (x,y) field

up :: Position -> Position
up (x, y) = (x, y-1)

down :: Position -> Position
down (x, y) = (x, y+1)

left :: Position -> Position
left (x, y) = (x-1, y)

right :: Position -> Position
right (x, y) = (x+1, y)

makeObscure :: Square -> Square
makeObscure sqr = sqr { sqrVisibility=False }

makeVisible :: Square -> Square
makeVisible sqr = sqr { sqrVisibility=True }

mapField :: (Square -> Square) -> Field -> Field
mapField func field = field { fieldRows = newRows }
    where oldRows = fieldRows field
          newRows = map (map func) oldRows

safeContainsObject :: Field -> Position -> Bool
safeContainsObject = safeSquareCheck containsObject


safeIsObscure :: Field -> Position -> Bool
safeIsObscure = safeSquareCheck (not . sqrVisibility) 


safeIsEnd :: Field -> Position -> Bool
safeIsEnd = safeSquareCheck sqrEnd

safeSquareCheck :: (Square -> Bool) -> (Field -> Position -> Bool)
safeSquareCheck func = newFunc 
    where newFunc field p = isJust value && fromJust value
            where value = safeGetSquare p field >>= (Just . func)
