module Field where

-- Haskell Core Imports
import Control.Concurrent.MVar
import qualified Data.Map as Map
import Control.Monad (liftM, liftM2)
import Data.Maybe


-- Module Imports
import Sprite
import Square


-- Data Definitions
data Field = Field { fldSize :: Position 
                   , fldMap :: Map.Map Position Square
                   }


-- Creation Functions
newField :: Position -> IO Field
newField p = do 
    fieldMap <- createFieldMap p 
    return Field { fldSize=p
                 , fldMap=fieldMap
                 }


createFieldMap :: Position -> IO (Map.Map Position Square)
createFieldMap (width, height) = list >>= return . Map.fromList 
    where list = sequence
                  [ liftM2 (\a b -> (a, b)) (return (x, y)) (newSquare (x, y))
                  | x <- [1..width], y <- [1..height]
                  ]


-- Query Functions
fieldRows :: Field -> [[Square]]
fieldRows fld = [ [ fromJust $ Map.lookup (x, y) fieldMap | x <- [1..width] ]
                  | y <- [1..height]
                ]
    where (width, height) = fldSize fld
          fieldMap = fldMap fld


getSquare :: Field -> Position -> Maybe Square
getSquare fld pos = Map.lookup pos (fldMap fld)


getSquare_ :: Field -> Position -> Square
getSquare_ fld pos = fromJust $ Map.lookup pos (fldMap fld)


validPos :: Field -> Position -> Bool
validPos fld = isJust . getSquare fld


-- Manipulate Functions
up :: Position -> Position
up (x, y) = (x, y-1)
    -- not a bug, top left is (1, 1) bottom right is (n,m)


down :: Position -> Position
down (x, y) = (x, y+1)


left :: Position -> Position
left (x, y) = (x-1, y)


right :: Position -> Position
right (x, y) = (x+1, y)


move :: Field -> Position -> Position -> IO Bool
move fld start end = do
    if (validPos fld start) && (validPos fld end)
       then do
        let firstSquare = getSquare_ fld start
        let secondSquare = getSquare_ fld end
        mover <- takeOccupant firstSquare
        if isJust mover
           then do 
               putOccupant (fromJust mover) secondSquare
               return True
           else return False 
       else return False


moveUp :: Field -> Position -> IO Bool
moveUp fld start = move fld start (up start)


moveDown :: Field -> Position -> IO Bool
moveDown fld start = move fld start (down start)


moveLeft :: Field -> Position -> IO Bool
moveLeft fld start = move fld start (left start)


moveRight :: Field -> Position -> IO Bool
moveRight fld start = move fld start (right start)


place :: Field -> Position -> Occupant -> IO Bool
place fld pos occ = do
    if validPos fld pos
       then do
           let square = getSquare_ fld pos
           putOccupant occ square
           return True
       else return False


placeObject :: Field -> Position -> IO Bool
placeObject fld pos = place fld pos ObjectOccupant


placeSprite :: Field -> Position -> Sprite -> IO (Maybe SpriteContainer)
placeSprite fld pos spr = do
    spriteContainer <- newSpriteContainer spr (Just pos)
    placed <- place fld pos (SpriteOccupant spriteContainer) 
    if placed
       then return $ Just $ spriteContainer
       else return Nothing


moveSprite :: Field -> (Position -> Position) -> SpriteContainer -> IO Bool
moveSprite fld movement sprCon = do
    mSpritePosition <- extractSpritePosition sprCon
    if isNothing mSpritePosition
       then return False 
       else do
        let spritePosition = fromJust mSpritePosition
        let newPosition = movement spritePosition
        moved <- move fld spritePosition newPosition
        if moved
           then do 
               updateSpritePosition sprCon newPosition
               return True
           else return False
