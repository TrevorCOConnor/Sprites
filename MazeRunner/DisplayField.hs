import Graphics.Gloss

data DisplaySquare = { dsLocation :: (Int, Int)
                     , dsPicture :: Picture
                     }

data DisplayField = { displayWidth :: Int
                    , displayLength :: Int
                    , displaySquares :: [[DisplaySquare]]
                    }


createDisplayField :: (Int, Int) -> Picture -> DisplayField
createDisplayField (w, l) p = DisplayField { displayWidth=w
                                         , displayLength=l
                                         , displaySquares=[dRows]
                                         }
    where dRows = createDisplayRows (w, l) p


createDisplayRows :: (Int, Int) -> Picture -> [DisplaySquare]
createDisplayRows (w, l) p = [ displayRow y | y <- [0..(l-1)]
    where displayRow y = [ createDisplaySquare (w, l) | x <- [0..(w-1] ]
