module Movement where

import Field

type Position = (Int, Int)

move :: Position -> Position -> Field -> (Position, Field)
move p1 p2 field = if validPos p2 field
                      then (p2, newField) 
                      else (p1, field)
    where occ = sqrOccupant $ getSquare p1 field
          newField = modifySquare (placeOccupant occ) p2 . modifySquare makeVacant p1 $ field

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

modifySquare :: (Square -> Square) -> Position -> Field -> Field
modifySquare f (x, y) field = field { fieldRows = rs' }
    where rs' = bRows ++ r':aRows
          (bRows, r:aRows) = splitAt (y) (fieldRows field)
          (bCols, c:aCols) = splitAt (x) r
          c' = f c
          r' = bCols ++ c':aCols

placeObj :: Square -> Square
placeObj sqr = sqr { sqrOccupant = ContainsObject }

placeObjOnField :: Position -> Field -> Field
placeObjOnField = modifySquare placeObj

makeVacant :: Square -> Square
makeVacant sqr = sqr { sqrOccupant = Vacant }

isVacant :: Square -> Bool
isVacant = (Vacant ==) . sqrOccupant

placeOccupant :: Occupant -> Square -> Square
placeOccupant o sqr = sqr { sqrOccupant = o }

validPos :: Position -> Field -> Bool
validPos (x, y) field = xvalid && yvalid && isVacant sqr
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
