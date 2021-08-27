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
