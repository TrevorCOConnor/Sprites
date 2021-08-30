module TerminalDisplay where

-- Core Haskell
import Control.Concurrent.MVar
import Control.Monad

-- Internal Modules
import Field
import Square
import Sprite


-- Helper Functions
bufferStringLeft :: Int -> String -> String
bufferStringLeft size txt = (replicate (size - len) ' ') ++ (take len txt)
    where len = min (length txt) size

bufferStringRight :: Int -> String -> String
bufferStringRight size txt = (take len txt) ++ (replicate (size - len) ' ')
    where len = min (length txt) size


-- Display Functions
displaySquare :: Square -> IO (String)
displaySquare sqr = do
    occupied <- isOccupiedSquare sqr
    if occupied
        then (liftM show) . readMVar . sqrContent $ sqr 
        else return " "


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


displayableField :: Field -> IO String
displayableField fld = do
    displayRows <- sequence $ map (sequence . (map displaySquare)) (fieldRows fld) 
    let cap = '+' : replicate (fst (fldSize fld)) '-' ++ "+"
    let concatRows = map concat displayRows
    let enumeratedRows = enumerateRows concatRows
    let unlinedRows = unlines enumeratedRows
    return $ cap ++ "\n" ++ unlinedRows ++ cap
