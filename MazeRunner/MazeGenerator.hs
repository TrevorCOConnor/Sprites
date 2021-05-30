module MazeGenerator where

import System.Random 
import Control.Monad (liftM2)
import Vision

data LexOrder = XFirst | YFirst
    deriving (Eq)

instance Ord LexOrder where
    XFirst <= YFirst = True
    YFirst <= XFirst = False

instance Random LexOrder where
    randomR (XFirst, YFirst) g = if v
                                    then (XFirst, g1)
                                    else (YFirst, g1)
        where (v, g1) = randomR(False, True) g

    random g = if v
                  then (XFirst, g1)
                  else (YFirst, g1)
        where (v, g1) = randomR(False, True) g

type Size = Int
type Width = Int
type Height = Int
type Min = Int
type Max = Int
type Position = (Int, Int)


randomPath :: Width -> Height -> IO [Position]
randomPath w h = do 
    let w' = w `div` 2 
    let h' = h `div` 2
    lst <- randomList w' h'
    orderedLst <- randomOrderPoints ((0,0): lst)
    let path = joinPath orderedLst
    let finishedPath = createEnding w h path
    return $ cleansePath finishedPath

createListOfPoints :: Width -> Height -> [Position]
createListOfPoints w h = listOfPoints 
    where (start : listOfPoints) = [(a, b) | a <- [0..(w-1)], b <- [0..(h-1)]]


randomPoint :: Width -> Height -> IO Position
randomPoint w h = do
    x <- randomRIO (0, w)
    y <- randomRIO (0, h) 
    return (x, y)


randomElemRemoval :: [a] -> IO (a, [a])
randomElemRemoval [] = error "empty list"
randomElemRemoval lst = do
    let max = (length lst) - 1
    pos <- randomRIO (0, max)
    let (beg, (elem : end)) = splitAt pos lst
    return (elem, beg ++ end)


maxPercentage :: Int
maxPercentage = 10


minPercentage :: Int
minPercentage = 5


randomList :: Width -> Height -> IO [Position]
randomList w h = do
    let lst = createListOfPoints w h
    let max = (w*h) `div` (100 `div` (maxPercentage*2))
    let min = (w*h) `div` (100 `div` (minPercentage*2)) 
    n <- randomRIO (min, max)
    randomList' n lst


randomList' :: Int -> [a] -> IO [a]
randomList' 0 _          = return []
randomList' n remaining  = do
    (elem, rem) <- randomElemRemoval remaining
    rest <- randomList' (n-1) rem
    return $ elem : rest


randomOrderPoints :: [Position] -> IO [Position]
randomOrderPoints [] = return []
randomOrderPoints (p: []) = return [p]
randomOrderPoints (p: ps) = do
    ord <- randomIO :: IO LexOrder
    let (next, rem) = takeNextLexOrder ord p ps  
    rest <- randomOrderPoints (next: rem)
    return $ p : rest


(.<) :: Position -> Position -> Bool
(a, b) .< (x, y) = (a < x) || (a == x && b < y) 


(<.) :: Position -> Position -> Bool
(a, b) <. (x, y) = (b < y) || (b == y && a < x) 


lexDisp :: Position -> Position -> Position
lexDisp (a, b) (x, y) = (abs (a-x), abs (b-y))


takeNextLexOrder :: LexOrder -> Position -> [Position] -> (Position, [Position])
takeNextLexOrder lexOrder pos lst = (next, filter (/= next) lst)  
    where next = nextLexOrder lexOrder pos lst


nextLexOrder :: LexOrder -> Position -> [Position] -> Position
nextLexOrder lexOrder pos [] = error "empty list" 
nextLexOrder lexOrder pos lst = minLexDisp lexOrder ranked 
    where disps = map (lexDisp pos) lst
          ranked = zip disps lst
          

minLexDisp :: LexOrder -> [(Position, Position)] -> Position
minLexDisp lexOrder [] = error "empty list"
minLexDisp lexOrder (e: []) = snd e
minLexDisp lexOrder (e: es) = minLexDisp lexOrder $ filter (`lt` e) es ++ [e]
    where lt x y = case lexOrder of
                        XFirst -> (fst x) .< (fst y)
                        YFirst -> (fst x) <. (fst y) 


connectPoints :: Position -> Position -> [Position]
connectPoints p1@(a, b) p2@(x, y) = if abs (x-a) <= abs (y-b)
                                       then tail xFirst
                                       else tail yFirst
    where xFirst = xpath ++ ypath
                    where xpath = xPath p1 p2
                          ypath = yPath (last xpath) p2
          yFirst = ypath ++ xpath
                    where ypath = yPath p1 p2
                          xpath = xPath (last ypath) p2


xPath :: Position -> Position -> [Position]
xPath (a, b) (x, _) = [(i, b) | i <- path]
    where path | a < x = [a..x]
               | a > x = reverse [x..a]
               | otherwise = [a]


yPath :: Position -> Position -> [Position]
yPath (a, b) (_, y) = [(a, i) | i <- path]
    where path | b < y = [b..y]
               | b > y = reverse [y..b]
               | otherwise = [b]


joinPath :: [Position] -> [Position]
joinPath ps = concat $ map (\(x, y) -> connectPoints x y) $ zip ps' (tail ps')
    where ps' = map (\(x,y) -> (2*x, 2*y)) ps


createEnding :: Width -> Height -> [Position] -> [Position]
createEnding w h path = if x <= y
                           then path ++ connectPoints end (w', snd end)
                           else if up <= down
                                    then path ++ connectPoints end (fst end, 0)
                                    else path ++ connectPoints end (fst end, h')
    where h' = h - 1 
          w' = w - 1
          end = last path
          up = snd end
          down = h' - snd end
          x = w' - fst end
          y = min up down


cleansePath :: [Position] -> [Position] 
cleansePath = reverse . (cleansePath' [])


cleansePath' :: [Position] -> [Position] -> [Position]
cleansePath' checked [] = checked
cleansePath' checked (p1:[]) = p1:checked
cleansePath' checked (p1:p2:ps) = if p2 `elem` checked
                                     then if oddPos p1
                                             then cleansePath' checked ps
                                             else cleansePath' (p1:checked) ps
                                     else cleansePath' (p2:p1:checked) ps

oddPos :: Position -> Bool
oddPos (x, y) = (x `mod` 2) == 1 || (y `mod` 2 == 1)
