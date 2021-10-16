module Field where

-- Haskell Core Imports
import Control.Concurrent.MVar
import qualified Data.Map as Map
import Control.Monad (liftM, liftM2, liftM3, join)
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


greenStartingPositions :: Position -> [Position]
greenStartingPositions fieldSize = [ (x, y) | x <- [1, 3 .. (fst fieldSize)]
                                            , y <- [1..(halfField)] ]
    where halfField = (snd fieldSize) `div` 2


redStartingPositions :: Position -> [Position] 
redStartingPositions fieldSize = [ (x, y) | x <- reverse $ [1, 3 .. (fst fieldSize)]
                                          , y <- reverse $ [halfField..(snd fieldSize)] ]
    where halfField = (snd fieldSize) `div` 2


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
    let firstSquare = getSquare fld start
    let secondSquare = getSquare fld end
    mover <- sequence (liftM takeOccupant firstSquare) >>= return . join
    maybePut <- sequence $ liftM2 putOccupant mover secondSquare 
    let put = fromMaybe False maybePut
    if put
       then return True
       else do
           sequence $ liftM2 putOccupant mover firstSquare 
           return False


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
    let square = getSquare fld pos
    put <- sequence $ liftM2 putOccupant (Just occ) square
    return $ fromMaybe False put


placeObject :: Field -> Position -> IO Bool
placeObject fld pos = place fld pos ObjectOccupant


placeSprite :: Field -> Position -> Sprite -> Team -> IO (Maybe SpriteContainer)
placeSprite fld pos spr team = do
    spriteContainer <- newSpriteContainer spr team (Just pos)
    placed <- place fld pos (SpriteOccupant spriteContainer) 
    if placed
       then return $ Just $ spriteContainer
       else return Nothing


placeGreenSprite :: Field -> Position -> Sprite -> IO (Maybe SpriteContainer)
placeGreenSprite fld pos spr = placeSprite fld pos spr GreenTeam


placeRedSprite :: Field -> Position -> Sprite -> IO (Maybe SpriteContainer)
placeRedSprite fld pos spr = placeSprite fld pos spr RedTeam


moveSprite :: Field -> (Position -> Position) -> SpriteContainer -> IO Bool
moveSprite fld movement sprCon = do
    containerPosition <- getContainerPosition sprCon
    let newPosition = liftM movement containerPosition
    moved <- sequence $ liftM3 move (Just fld) containerPosition newPosition
    if fromMaybe False moved
        then do
            updateContainerPosition sprCon (fromJust newPosition)
            return True
        else return False
