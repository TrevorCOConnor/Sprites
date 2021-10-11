module TerminalDisplay where

-- Core Haskell
import Control.Concurrent.MVar
import Control.Monad
import Data.Maybe
import System.Console.ANSI


-- Internal Modules
import Field
import Square
import Sprite
import Ansi
import Roster
import BattleState


-- Helper Functions
bufferStringLeft :: Int -> String -> String
bufferStringLeft size txt = (replicate (size - len) ' ') ++ (ansiTake len txt)
    where len = min (ansiLength txt) size


bufferStringRight :: Int -> String -> String
bufferStringRight size txt = (ansiTake len txt) ++ (replicate (size - len) ' ')
    where len = min (ansiLength txt) size


bufferLinesRight :: Int -> [String] -> [String]
bufferLinesRight buffer = map (bufferStringRight buffer)


bufferNumLines :: Int -> [String] -> [String]
bufferNumLines n lines = lines ++ replicate (n - numLines) ""
    where numLines = length lines


mergeLines :: Int -> String -> String -> String
mergeLines buffer left right = unlines $ [x ++ y | (x, y) <- zip bufferedLeft rightLines'] 
    where leftLines = lines left
          rightLines = lines right
          maxLines = max (length leftLines) (length rightLines)
          leftLines' = bufferNumLines maxLines leftLines
          rightLines' = bufferNumLines maxLines rightLines
          maxLeft = maximum $ map ansiLength leftLines
          bufferedLeft = bufferLinesRight (maxLeft + buffer) leftLines'


-- Display Functions
spriteChar :: String
spriteChar = "S"


objectChar :: String
objectChar = "#"


vacantChar :: String
vacantChar = " "


displaySpriteContainer_ :: SpriteContainer_ -> String
displaySpriteContainer_ sprCon = 
    case _conTeam sprCon of
      GreenTeam -> applyColor green spriteChar
      RedTeam -> applyColor red spriteChar


displaySpriteContainer :: SpriteContainer -> IO String
displaySpriteContainer (SpriteContainer innerContainer) = do
    sprCon <- readMVar innerContainer
    return $ displaySpriteContainer_ sprCon


displayOccupant :: Occupant -> IO (String)
displayOccupant ObjectOccupant = return objectChar
displayOccupant (SpriteOccupant sprCon) = displaySpriteContainer sprCon


displaySquare :: Square -> IO (String)
displaySquare sqr = do
    occupant <- readOccupant sqr
    displayableOccupant <- sequence $ liftM displayOccupant occupant
    return $ fromMaybe " " displayableOccupant


-- Field
fieldCapBody :: Int -> String -> String
fieldCapBody size body = cap ++ '\n':body ++ cap 
    where cap = "  +" ++ replicate size '-' ++ "+"


fieldDisplayRow :: [Square] -> IO String
fieldDisplayRow = (liftM concat) . sequence . map displaySquare


fieldCapRows :: String -> String
fieldCapRows row = cap ++ row ++ cap
    where cap = "|"


enumerateRows :: [String] -> [String]
enumerateRows = map (\(e, r) -> (bufferStringLeft 2 e) ++ r) . zip (map show [1..])


fieldDisplay :: Field -> IO String
fieldDisplay = (liftM (unlines . enumerateRows))
                -- enumerate rows and unify to one string
                . sequence
                -- lift [IO String] -> IO [String]
                . (map ((liftM fieldCapRows) . fieldDisplayRow))
                -- Convert rows to displayable format and add caps
                . fieldRows 
                -- get field rows


cappedFieldDisplay :: Field -> IO String
cappedFieldDisplay fld = liftM (fieldCapBody fldWidth) . fieldDisplay $ fld 
    where fldWidth = fst . fldSize $ fld


displayField :: Field -> IO ()
displayField fld = do
    clearScreen
    cappedFieldDisplay fld >>= putStrLn

-- BattleState
mergeBuffer :: Int
mergeBuffer = 3


displayBattleState :: BattleState -> IO String
displayBattleState (BattleState field fRoster) = do
    fieldString <- cappedFieldDisplay field
    rosterString <- displayFullRoster fRoster 
    return $ mergeLines mergeBuffer fieldString rosterString
