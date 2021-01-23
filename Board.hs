module Board where

import SpriteData
import Data.Either

data Square = Square { sqrPos   :: Position 
                     , occupant :: Occupant 
                     , effects  :: [Effect]
                     }

data Occupant = Vacant | ContainSprite BattleSprite | ContainObject

instance Eq Occupant where
    Vacant          == Vacant          = True
    ContainSprite _ == ContainSprite _ = True
    ContainObject   == ContainObject   = True
    _               == _               = False

type Position = (Int, Int)

instance Show Square where
    show square = case occupant square of
          Vacant          -> "[ ]"
          ContainSprite _ -> "[S]"
          ContainObject   -> "[#]"

newSquare :: (Int, Int) -> Square
newSquare pos = Square { sqrPos   = pos
                       , occupant = Vacant
                       , effects  = []
                       }

placeObj :: Square -> Square 
placeObj sqr = sqr { occupant = ContainObject }

placeSprite :: BattleSprite -> Square -> Square
placeSprite bSprite sqr = sqr { occupant = ContainSprite bSprite }

makeVacant :: Square -> Square
makeVacant sqr = sqr { occupant = Vacant }

placeOccupant :: Occupant -> Square -> Square
placeOccupant o sqr = sqr { occupant = o }

data Board = Board Width Length BoardSquares

type Length       = Int
type Width        = Int
type BoardSquares = [[Square]]

getRows :: Board -> BoardSquares
getRows (Board _ _ rows) = rows

createRow :: Width -> Length -> [Square]
createRow w l | w <= 0     = []
              | l <= 0     = []
              | otherwise  = newSquare (w, l) : createRow (w-1) l

createBoardSquares :: Width -> Length -> BoardSquares
createBoardSquares w 0 = []
createBoardSquares w l = createRow w l : createBoardSquares w (l-1)

newBoard :: Width -> Length -> Board
newBoard w l = Board w l $ createBoardSquares w l

showRow :: [Square] -> String
showRow []       = ""
showRow (sq:sqs) = show sq ++ showRow sqs 

fillSpace :: String -> Int -> String
fillSpace text max = replicate spaceLength ' '
    where spaceLength = max - length text

showColumnHeader :: Width -> String
showColumnHeader w = ('\n':replicate 4 ' ') ++ inner [1..w]
    -- Both showColumnHeader and Show Board assume the max dimension is 2 digits in length
    where inner []     = "\n"
          inner (i:is) = show i 
                         ++ fillSpace (show i) 3 
                         ++ inner is

instance Show Board where
    show (Board w l rows) = showColumnHeader w ++ inner 1 rows
    -- Both showColumnHeader and Show Board assume the max dimension is 2 digits in length
        where inner l []     = ""
              inner l (r:rs) = fillSpace (show l) 3 
                               ++ show l 
                               ++ " " 
                               ++ showRow r 
                               ++ "\n" 
                               ++ inner (l+1) rs 

getSquare :: Position -> Board -> Square
getSquare (x, y) board = row !! (x-1)
    where rows   = getRows board
          row    = rows !! (y-1)

modifySquare :: Position -> Board -> (Square -> Square) -> Board
modifySquare (x, y) (Board w l rows) f = Board w l rows'
    where rows' = bRows ++ (r':aRows)

          (bRows, r:aRows) = splitAt (y-1) rows
          (bCols, c:aCols) = splitAt (x-1) r

          c' = f c
          r' = bCols ++ (c':aCols)
          
updateSquare :: Square -> Board -> Board
updateSquare sqr b = modifySquare pos b (const sqr)
    where pos = sqrPos sqr

placeSpriteOnBoard :: Position -> BattleSprite -> Board -> Board
placeSpriteOnBoard p bSprite b = modifySquare p b (placeSprite bSprite)

placeObjOnBoard :: Position -> Board -> Board
placeObjOnBoard p b = modifySquare p b placeObj

placeObjsOnBoard :: [Position] -> Board -> Board
placeObjsOnBoard ps b = foldr placeObjOnBoard b ps

validPosition :: Position -> Board -> (Position -> Board -> a) -> Either String a
validPosition p@(x,y) b@(Board w l _) f | x <= 0 || w <= x = errorMessage
                                        | y <= 0 || l <= y = errorMessage
                                        | otherwise        = Right $ f p b
    where errorMessage = Left $ "Position " ++ show p ++ " is not valid."

isVacant :: Position -> Board -> Bool
isVacant (x, y) (Board _ _ rows) = occupant col == Vacant
    where row = rows !! y
          col = row !! x

move :: Position -> Position -> Board -> Board
move (a, b) (x, y) board = modifySquare (x, y) board' $ placeOccupant occ
    where occ    = occupant $ getSquare (a, b) board
          board' = modifySquare (a, b) board makeVacant

moveUp :: Position -> Board -> Board
moveUp (x,y) board = move (x, y) (x, y-1) board 

moveDown :: Position -> Board -> Board
moveDown (x,y) board = move (x, y) (x, y+1) board 

moveLeft :: Position -> Board -> Board
moveLeft (x,y) board = move (x, y) (x-1, y) board 

moveRight :: Position -> Board -> Board
moveRight (x,y) board = move (x, y) (x+1, y) board 

moveSpriteRight :: (BattleSprite, Board) -> (BattleSprite, Board)
moveSpriteRight (bSpr, b) = (bSpr', moveRight (x,y) b) 
    where bSpr' = bSpr { battlePosition = (x+1,y) }
          (x,y) = battlePosition bSpr

moveSpriteLeft :: (BattleSprite, Board) -> (BattleSprite, Board)
moveSpriteLeft (bSpr, b) = (bSpr', moveLeft (x,y) b) 
    where bSpr' = bSpr { battlePosition = (x-1,y) }
          (x,y) = battlePosition bSpr

moveSpriteUp :: (BattleSprite, Board) -> (BattleSprite, Board)
moveSpriteUp (bSpr, b) = (bSpr', moveUp (x,y) b) 
    where bSpr' = bSpr { battlePosition = (x,y-1) }
          (x,y) = battlePosition bSpr

moveSpriteDown :: (BattleSprite, Board) -> (BattleSprite, Board)
moveSpriteDown (bSpr, b) = (bSpr', moveDown (x,y) b) 
    where bSpr' = bSpr { battlePosition = (x,y+1) }
          (x,y) = battlePosition bSpr
