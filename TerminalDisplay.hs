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



-- Helper Functions
bufferStringLeft :: Int -> String -> String
bufferStringLeft size txt = (replicate (size - len) ' ') ++ (take len txt)
    where len = min (length txt) size

bufferStringRight :: Int -> String -> String
bufferStringRight size txt = (take len txt) ++ (replicate (size - len) ' ')
    where len = min (length txt) size


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
