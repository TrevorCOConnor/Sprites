module Field
    ( Field(..)
    , createField
    , Square(..)
    , Occupant(..)
    ) where

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


data Square = Square { sqrPosition :: (Int, Int)
                     , sqrOccupant :: Occupant }

instance Show Square where
    show sqr = show $ sqrOccupant sqr


data Occupant = Vacant | ContainsObject
    deriving Eq

instance Show Occupant where
    show Vacant = " "
    show ContainsObject = "#"

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
                             , sqrOccupant=Vacant }
