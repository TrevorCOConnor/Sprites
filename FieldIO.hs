import Field 
import Movement
import System.IO
import Control.Exception
import System.Console.ANSI


data ActionMap = ActionMap { key :: Char
                           , action :: IO (Position -> Field -> (Position, Field)) }

main :: IO()
main = do
    do hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    hideCursor
    let field = placeObjOnField (0,0) $ createField (60, 30)
    hClearScreen stdout
    setCursorPosition 0 0
    print field
    (pos, newField) <- moveOnField (0,0) field
    return ()

actionKeys :: IO (Position -> Field -> (Position, Field))
actionKeys = do
    char <- withEcho False getChar
    if char `elem` "wasd"
       then return $ mapAction char
       else actionKeys

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action


mapAction :: Char -> (Position -> Field -> (Position, Field))
mapAction c
    | c == 'w' = moveUp
    | c == 'a' = moveLeft
    | c == 's' = moveDown
    | c == 'd' = moveRight
    | otherwise = error "Unrecognized movement."


moveOnField :: Position -> Field -> IO (Position, Field)
moveOnField pos field = do
    movement <- readAction defaultActions
    let (newPos, newField) = movement pos field 
    setCursorPosition 0 0
    print newField
    moveOnField newPos newField


readAction :: [ActionMap] -> IO (Position -> Field -> (Position, Field)) 
readAction actionMaps = do
    char <- withEcho False getChar
    if char `elem` (map key actionMaps)
       then mapAction' actionMaps char
       else actionKeys


mapAction' :: [ActionMap] -> Char -> IO (Position -> Field -> (Position, Field))  
mapAction' [] _ = error "Unrecognized action."
mapAction' (aMap:ms) c = if key aMap == c 
                            then action aMap
                            else mapAction' ms c


defaultActions :: [ActionMap]
defaultActions = [ ActionMap {key='w', action=return moveUp}
                 , ActionMap {key='a', action=return moveLeft}
                 , ActionMap {key='s', action=return moveDown}
                 , ActionMap {key='d', action=return moveRight}
                 , ActionMap {key='p', action= readAction placeActions }
                 ]

placeActions :: [ActionMap]
placeActions = [ ActionMap {key='w', action=return $ modifiedPlaceObject up}
               , ActionMap {key='a', action=return $ modifiedPlaceObject left}
               , ActionMap {key='s', action=return $ modifiedPlaceObject down}
               , ActionMap {key='d', action=return $ modifiedPlaceObject right}
               ]

modifiedPlaceObject :: (Position -> Position) -> Position -> Field -> (Position, Field)
modifiedPlaceObject dir p field = (p, placeObjOnField p' field)
    where p' = dir p
