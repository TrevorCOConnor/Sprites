module FieldIO where
    
import View
import MazeGenerator (randomPath)
import Vision
import Actions
import Field 
import Movement
import System.IO
import Control.Exception
import System.Console.ANSI
import Control.Concurrent


radius :: Int
radius = 30


width :: Int
width = 100


height :: Int
height = 50


startingField :: Field
startingField = mapField placeObj field'
    where field' = createField (width, height)


applyPath :: [Position] -> Field -> Field
applyPath path field = foldr (makePositionVacant) field path


play :: IO()
play = do
    do hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    hideCursor
    randPath <- randomPath width height
    let start = head randPath
    let end = last randPath
    let field'' = applyPath randPath startingField
    let field' = placeEndOnField end field''
    let field = placePlayerOnField (head randPath) field'
    hClearScreen stdout
    setCursorPosition (fst start) (snd start)
    printVisualView start field
    actionLoop start field
    showCursor
    return ()


printField :: Field -> IO ()
printField field = do
    setCursorPosition 0 0
    print field


printVisualField :: Position -> Field -> IO ()
printVisualField pos = printField . linearVision' radius pos


vRadius :: Float
vRadius = 30


printView :: View -> IO ()
printView view = do
    setCursorPosition 0 0
    print view


printVisualView :: Position -> Field -> IO ()
printVisualView pos field = printView view
    where field' = linearVision' radius pos field
          view = View { viewRadius=vRadius 
                      , viewField=field'
                      , viewFocalPoint=pos
                      }
                             


withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action


actionLoop :: Position -> Field -> IO ()
actionLoop pos field = do
    action <- readAction defaultActions
    case action of
        Movement movement -> do 
            let (newPos, newField) = movement pos field 
            if sqrEnd $ getSquare newPos newField
               then do
                   hClearScreen stdout
                   putStrLn "You escaped!!!"
            else do
                printVisualView newPos newField
                actionLoop newPos newField
        ModifyField modification -> do
            let newField = modification pos field
            printVisualView pos newField
            actionLoop pos newField
        ModifyView view -> do
            let newField = view field
            printField newField
            threadDelay $ secondsToMilliseconds 3
            hClearScreen stdout
            printVisualView pos field
            actionLoop pos field
        Escape -> return()


readAction :: [ActionMap] -> IO Action
readAction actionMaps = do
    char <- withEcho False getChar
    if char `elem` (map key actionMaps)
       then mapAction actionMaps char
       else readAction actionMaps


mapAction :: [ActionMap] -> Char -> IO Action
mapAction [] _ = error "Unrecognized action."
mapAction (aMap:ms) c = if key aMap == c 
                            then action aMap
                            else mapAction ms c


defaultActions :: [ActionMap]
defaultActions = [ ActionMap {key='w', action=return $ Movement moveUp }
                 , ActionMap {key='a', action=return $ Movement moveLeft }
                 , ActionMap {key='s', action=return $ Movement moveDown }
                 , ActionMap {key='d', action=return $ Movement moveRight }
                 , ActionMap {key='p', action=readAction placeActions}
                 , ActionMap {key='r', action=readAction removeActions}
                 , ActionMap {key='v', action=return $ ModifyView (mapField makeVisible)}
                 , ActionMap {key='\ESC', action= return Escape}
                 ]


placeActions :: [ActionMap]
placeActions = [ ActionMap {key='w', action=return $ ModifyField $ placeObjOnField . up}
               , ActionMap {key='a', action=return $ ModifyField $ placeObjOnField . left}
               , ActionMap {key='s', action=return $ ModifyField $ placeObjOnField . down}
               , ActionMap {key='d', action=return $ ModifyField $ placeObjOnField . right}
               ]


removeActions :: [ActionMap]
removeActions = [ ActionMap {key='w', action=return $ ModifyField $ makePositionVacant . up}
                , ActionMap {key='a', action=return $ ModifyField $ makePositionVacant . left}
                , ActionMap {key='s', action=return $ ModifyField $ makePositionVacant . down}
                , ActionMap {key='d', action=return $ ModifyField $ makePositionVacant . right}
                ]


secondsToMilliseconds :: Float -> Int
secondsToMilliseconds s = round $ s*milli 
    where milli = 1000000.0
