module Square where

-- Core Haskell
import Control.Concurrent.MVar
import Control.Monad (liftM)


-- Internal Modules
import Sprite


-- Data Defintions
data Occupant = ObjectOccupant | SpriteOccupant SpriteContainer  

instance Show Occupant where
    show (ObjectOccupant) = "#"
    show (SpriteOccupant _) = "s"


data Square = Square { sqrPosition :: Position
                     , sqrContent :: MVar Occupant
                     }


-- Creation Functions
newSquare :: Position -> IO Square
newSquare p = do 
    squareContent <- newEmptyMVar
    return Square { sqrPosition=p
                  , sqrContent=squareContent
                  }


-- Query Functions
isOccupiedSquare :: Square -> IO Bool
isOccupiedSquare = (liftM not) . (isEmptyMVar . sqrContent)


readOccupant :: Square -> IO (Maybe Occupant)
readOccupant sqr = do
    let content = sqrContent sqr
    tryReadMVar content


-- Manipulate Functions
putOccupant :: Occupant -> Square -> IO Bool 
putOccupant occ sqr = do
    status <- tryPutMVar (sqrContent sqr) occ
    return status


takeOccupant :: Square -> IO (Maybe Occupant)
takeOccupant = tryTakeMVar . sqrContent


swapOccupant :: Occupant -> Square -> IO (Maybe Occupant)
swapOccupant newOcc sqr = do
    oldOcc <- takeOccupant sqr 
    putOccupant newOcc sqr
    return oldOcc
