import Control.Concurrent
import FieldIO
import MazeGenerator
import Field
import Movement (placePlayerOnField, placeObjOnField)
import System.Console.ANSI


main :: IO ()
main = do
    let w' = width `div` 2 
    let h' = height `div` 2
    lst <- randomList w' h'
    orderedLst <- randomOrderPoints ((0,0): lst)
    let corners = map rescalePoint orderedLst
    let path = joinPath orderedLst
    let finishedPath = createEnding width height path
    let cleanPath = cleansePath finishedPath
    let field = createField (width, height)
    let field' = foldr (\x y -> placePlayerOnField x y) field corners
    addStepToField field' corners cleanPath
    print corners


rescalePoint :: Position -> Position
rescalePoint (x, y) = (x*2, y*2)


addStepToField :: Field -> [Position] -> [Position] -> IO ()
addStepToField _ _ [] = return()
addStepToField field corners (s: ss) = do
    if s `elem` corners
       then addStepToField field corners ss
       else do
           setCursorPosition 0 0
           let field' = placeObjOnField s field
           print field
           -- threadDelay $ secondsToMilliseconds 0.05
           addStepToField field' corners ss
