module Field
    ( Field(..)
    , createField
    , showMarkedField
    , Square(..)
    , Occupant(..)
    ) where

import Data.Maybe

data Field = Field { fieldWidth  :: Width
                   , fieldLength :: Length
                   , fieldRows   :: [[Square]]
                   }

instance Show Field where
    show field = ' ' : replicate (fieldWidth field) '='
        ++ (concat $ map showRow $ fieldRows field)
        ++ "\n "
        ++ replicate (fieldWidth field) '='
            where showRow row = "\n|" ++ concat ( map show row ) ++ "|"


showRow :: [Square] -> String
showRow row = "\n|" ++ concat ( map show row ) ++ "|"


mapShowRow :: [[Square]] -> String
mapShowRow rows = concat $ map showRow rows


data Square = Square { sqrPosition   :: (Int, Int)
                     , sqrOccupant   :: Occupant 
                     , sqrEnd        :: Bool
                     , sqrVisibility :: Bool
                     , sqrMark       :: Maybe Char
                     , sqrTraversed  :: Bool}

instance Show Square where
    show sqr = if sqrVisibility sqr
                  then if sqrEnd sqr 
                          then "E"
                          else show $ sqrOccupant sqr
                  else "#"


data Occupant = Vacant | ContainsObject | ContainsPlayer
    deriving Eq

instance Show Occupant where
    show Vacant = " "
    show ContainsObject = "+"
    show ContainsPlayer = "P"

type Length = Int
type Width  = Int


createField :: (Int, Int) -> Field
createField (x, y) = Field { fieldWidth=x
                           , fieldLength=y
                           , fieldRows=rows }
        where rows = createRows (x, y)


createRows :: (Int, Int) -> [[Square]]
createRows = createRows' 0
    where createRows' n (x, y) =
            if n >= y
               then []
               else createRowOfSquares (x, n) : createRows' (n+1) (x, y)


createRowOfSquares :: (Int, Int) -> [Square]
createRowOfSquares = createRowOfSquares' 0 
    where createRowOfSquares' n (x, y) =
            if n >= x
               then []
               else createSquare (n, y) : createRowOfSquares' (n+1) (x, y)


createSquare :: (Int, Int) -> Square
createSquare (x, y) = Square { sqrPosition=(x, y)
                             , sqrOccupant=Vacant 
                             , sqrEnd=False
                             , sqrVisibility=True
                             , sqrMark=Nothing
                             , sqrTraversed=False}

showMarkedField :: Field -> String
showMarkedField field = ' ' : replicate (fieldWidth field) '='
        ++ (concat $ map showMarkedRow $ fieldRows field)
        ++ "\n "
        ++ replicate (fieldWidth field) '═'
            where showRow row = "\n|" ++ concat ( map show row ) ++ "║"


showMarkedRow :: [Square] -> String
showMarkedRow row = "\n|" ++ concat ( map showMarkedSquare row ) ++ "|"


showMarkedSquare :: Square -> String
showMarkedSquare sqr = if sqrVisibility sqr
                          then sqrShow
                          else "#"
    where sqrShow 
            | sqrEnd sqr = "E"
            | sqrOccupant sqr /= Vacant = show $ sqrOccupant sqr
            | isJust $ sqrMark sqr = show $ fromJust $ sqrMark sqr  
            | otherwise = show $ sqrOccupant sqr
