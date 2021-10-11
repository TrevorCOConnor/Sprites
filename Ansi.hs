module Ansi where

-- Local modules
import Sprite


-- Colors
newtype AnsiColor = AnsiColor String
escapeChar :: Char
escapeChar = '\ESC' 


reset :: String
reset = escapeChar : "[0m"


green :: AnsiColor
green = AnsiColor $ escapeChar : "[32m"


backgroundGreen :: AnsiColor
backgroundGreen = AnsiColor $ escapeChar : "[42m"


backgroundWhite :: AnsiColor
backgroundWhite = AnsiColor $ escapeChar : "[47m"


red :: AnsiColor
red = AnsiColor $ escapeChar : "[31m"


-- Blocks

fullBlock :: Char
fullBlock =  '\9608'


sevenEigthBlock :: Char
sevenEigthBlock = '\9609'


threeFourthBlock :: Char
threeFourthBlock =  '\9610'


fiveEigthBlock :: Char
fiveEigthBlock =  '\9611'


halfBlock :: Char
halfBlock =  '\9612'


threeEigthBlock :: Char
threeEigthBlock =  '\9613'


oneFourthBlock :: Char
oneFourthBlock = '\9614'


oneEigthBlock :: Char
oneEigthBlock =  '\9615'


-- Functions
applyColor :: AnsiColor -> String -> String
applyColor (AnsiColor color) str = color ++ str ++ reset


teamToColor :: Team -> AnsiColor
teamToColor RedTeam = red
teamToColor GreenTeam = green


numToBlock :: Int -> Char
numToBlock 0 = ' '
numToBlock 1 = oneEigthBlock
numToBlock 2 = threeFourthBlock
numToBlock 3 = fiveEigthBlock
numToBlcok 4 = sevenEigthBlock 


ansiLength :: String -> Int
ansiLength [] = 0
ansiLength ('\ESC':'[':ss) = ansiLength $ tail $ dropWhile (/= 'm') ss ++ []
ansiLength (s:ss) = 1 + ansiLength ss


ansiTake :: Int -> String -> String
ansiTake size = if size <= 0 
                    then const []
                    else ansiTake_ size


ansiTake_ :: Int -> String -> String
ansiTake_ 0 _ = []
ansiTake_ _ [] = []
ansiTake_ n ('\ESC':'[':ss) = '\ESC':'[':before ++ ansiTake_ (n+1) after
    where (before, after) = break (== 'm') ss
ansiTake_ n (s: ss) = s : ansiTake_ (n-1) ss
