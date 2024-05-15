module FieldIO where
    
import View
import MazeGenerator (randomPath)
import Vision
import Actions
import Field 
import Movement
import DisplayMethod
import System.IO
import Control.Exception
import System.Console.ANSI
import Control.Concurrent
import Data.Maybe


data FieldState = FieldState { fsPosition :: Position
                             , fsField :: Field
                             , fsDisplayMethod :: DisplayMethod 
                             }

data ActionResult = FieldStateChange FieldState            |
                    ActionMapChange [ActionMap]            |
                    PartialApplication ActionResult Action |
                    NoResult                               |
                    Break

data ActionState = TerminateActions | ActionState { stateFieldState :: FieldState
                                                  , stateActionMaps :: [ActionMap]
                                                  }

movementKeys = MovementKeys { upKey = 'w'
                            , downKey = 's'
                            , leftKey = 'a'
                            , rightKey = 'd'
                            }

displayMethods = [ visualMethod
                 , linearMethod
                 , easyMethod
                 ]

width :: Int
width = 100


height :: Int
height = 45


startingField :: Field
startingField = mapField placeObj field'
    where field' = createField (width, height)


applyPath :: [Position] -> Field -> Field
applyPath path field = foldr (makePositionVacant) field path


defaultDisplayMethod :: DisplayMethod
defaultDisplayMethod = visualMethod


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
    let fs = FieldState { fsField=field
                        , fsDisplayMethod=defaultDisplayMethod
                        , fsPosition=start
                        }
    printDisplayMethod fs
    actionLoop ActionState{ stateActionMaps=defaultActions, stateFieldState=fs }
    showCursor
    return ()


printDisplayMethod :: FieldState -> IO () 
printDisplayMethod fs = do
    setCursorPosition 0 0
    let dm = fsDisplayMethod fs
    let pos = fsPosition fs
    let field = fsField fs
    method dm pos field


withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action


actionLoop :: ActionState -> IO () 
actionLoop astate = do
    action <- readAction $ stateActionMaps astate
    result <- applyAction (stateFieldState astate) action
    newState <- handleResult astate result 
    case newState of
      TerminateActions -> return ()
      _ -> actionLoop newState


handleResult :: ActionState -> ActionResult -> IO (ActionState)
handleResult _ Break = return TerminateActions 
handleResult astate result =
    case result of
      FieldStateChange fs -> return $ astate { stateFieldState=fs }
      ActionMapChange amaps -> return $ astate { stateActionMaps=amaps }
      NoResult -> return $ astate
      PartialApplication fstResult sndAction -> do
          astate' <- handleResult astate fstResult
          let fs = stateFieldState astate'
          result <- applyAction fs sndAction
          handleResult astate' result


applyAction :: FieldState -> Action -> IO (ActionResult)
applyAction fs action =
    case action of
        Movement movement -> do 
            let pos = fsPosition fs
            let field = fsField fs
            let (newPos, newField) = movement pos field 
            if sqrEnd $ getSquare newPos newField
               then do
                   hClearScreen stdout
                   putStrLn "You escaped!!!"
                   return Break
            else do
                let newFS = fs { fsPosition=newPos, fsField=newField }
                printDisplayMethod newFS
                return $ FieldStateChange newFS
        ModifyField modification -> do
            let pos = fsPosition fs
            let field = fsField fs
            let newFS = fs { fsField=modification pos field }
            printDisplayMethod newFS
            return $ FieldStateChange newFS
        ModifyView view -> do
            let newField = view $ fsField fs
            printDisplayMethod $ fs { fsDisplayMethod=easyMethod }
            _ <- getChar
            hClearScreen stdout
            printDisplayMethod fs
            return NoResult
        ChangeView -> do
            let display = fsDisplayMethod fs
            let newDisplay = toggleView displayMethods display
            hClearScreen stdout
            let newFS = fs { fsDisplayMethod=newDisplay }
            printDisplayMethod newFS
            return $ FieldStateChange newFS
        ToggleTraversed -> do
            let display = fsDisplayMethod fs
            let newDisplay = display { showTraversed = not (showTraversed display) }
            let newFS = fs { fsDisplayMethod=newDisplay }
            printDisplayMethod newFS
            return $ FieldStateChange newFS
        EmptyAction -> do
            return NoResult
        JointAction act1 act2 -> do
            result <- applyAction fs act1
            return $ PartialApplication result act2
        Escape -> return Break
        DirectionMap func -> do
            let amaps = func movementKeys
            newAction <- readAction amaps
            applyAction fs newAction


readAction :: [ActionMap] -> IO Action
readAction actionMaps = do
    char <- withEcho False getChar
    let act = lookup char $ toMap actionMaps
    if isJust act
       then return $ fromJust act
       else return EmptyAction


movementActions :: [ActionMap]
movementActions = createMovementMap movementKeys


defaultActions :: [ActionMap]
defaultActions = movementActions ++
                 [ ActionMap {key='v', action=ModifyView (mapField makeVisible)}
                 , ActionMap {key='V', action=ChangeView }
                 , ActionMap {key='x', action=markAction }
                 , ActionMap {key='r', action=removeObjAction }
                 , ActionMap {key='T', action=ToggleTraversed }
                 , ActionMap {key='\ESC', action=Escape}
                 ]


toggleView :: [DisplayMethod] -> DisplayMethod -> DisplayMethod
toggleView [] _ = defaultDisplayMethod
toggleView ds current = if isNothing next
                   then head ds
                   else fromJust next 
    where next = toggleView' ds current


toggleView' :: [DisplayMethod] -> DisplayMethod -> Maybe DisplayMethod
toggleView' [] _ = Nothing
toggleView' (d: ds) current = if d == current
                                 then safeHead ds
                                 else toggleView' ds current


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a: as) = Just a


secondsToMilliseconds :: Float -> Int
secondsToMilliseconds s = round $ s*milli 
    where milli = 1000000.0
