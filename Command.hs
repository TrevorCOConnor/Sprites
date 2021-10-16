module Command where

import System.IO
import Data.Char
import Data.Maybe
import qualified Data.Map as M

data Command = Cancel | Response String

type CommandMap a = M.Map String a


cancelChars :: [String]
cancelChars = ["\ESC"]


getInput :: IO String
getInput = reverse <$> getInput' ""


getInput' :: String -> IO String
getInput' cs = do
    hSetEcho stdin False
    hSetBuffering stdin NoBuffering   
    char <- getChar
    more <- hReady stdin
    if more
       then getInput' (char:cs)
       else return $ char:cs


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
exampleMap = M.fromList [ ("w", "Up")
                        , ("a", "Left")
                        , ("s", "Down")
                        , ("d", "Right")
                        , ("\ESC[D", "Left")
                        , ("\ESC[C", "Right")
                        , ("\ESC[A", "Up")
                        , ("\ESC[B", "Down")
                        ]
