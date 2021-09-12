module Command where

import System.IO
import Data.Char
import Data.Maybe
import qualified Data.Map as M

data Command = Cancel | Response String

type CommandMap a = M.Map Char a


cancelChars :: [Char]
cancelChars = "\ESC"


getInput :: IO Char
getInput = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    char <- getChar
    if char == '\ESC'
       then getChar
       else return $ toLower char


getInputLine :: IO String
getInputLine = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering
    getContents


getCommand :: CommandMap a -> IO a
getCommand map = do
    input <- getInput
    let command = M.lookup input map
    if isJust command
       then return (fromJust $ command)
       else getCommand map

getCommandMaybe :: CommandMap a -> IO (Maybe a)
getCommandMaybe map = do
    input <- getInput
    if input `elem` cancelChars
       then return Nothing
       else if M.member input map
           then return (M.lookup input map)
           else getCommandMaybe map


exampleMap :: CommandMap String
exampleMap = M.fromList [ ('w', "Up")
                        , ('a', "Left")
                        , ('s', "Down")
                        , ('d', "Right")
                        ]
