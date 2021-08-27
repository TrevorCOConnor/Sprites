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
isOccupiedSquare =  isEmptyMVar . sqrContent 


-- Manipulate Functions
putOccupant :: Occupant -> Square -> IO () 
putOccupant occ sqr = do
    _ <- tryPutMVar occ (sqrContent sqr)
    return ()


takeOccupant :: Square -> IO (Maybe Occupant)
takeOccupant = tryTakeMVar . sqrContent


swapOccupant :: Occupant -> Square -> IO (Maybe Occupant)
swapOccupant newOcc sqr = do
    oldOcc <- takeOccupant sqr 
    putOccupant newOcc sqr
    return oldOcc
