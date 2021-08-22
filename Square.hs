module Square where

-- Haskell Core Imports
import Control.Concurrent.MVar


-- Module Imports
import Sprite


-- Data Defintions
data Occupant = Vacant | ContainsSprite SpriteContainer  

instance Show Occupant where
    show (Vacant) = " "
    show (ContainsSprite _) = "s"


data Square = Square { sqrPosition :: Position
                     , sqrContent :: MVar Occupant
                     }


-- Creation Functions
newSquare :: Position -> IO Square
newSquare p = do 
    squareContent <- newMVar Vacant
    return Square { sqrPosition=p
                  , sqrContent=squareContent
                  }


-- Display Functions
displaySquare :: Square -> IO (String)
displaySquare sqr = (readMVar . sqrContent) sqr >>= return . show
