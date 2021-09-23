module Ansi where

-- Colors
newtype TColor = TColor String
escapeChar :: Char
escapeChar = '\ESC' 


reset :: String
reset = escapeChar : "[0m"


green :: TColor
green = TColor $ escapeChar : "[32m"


backgroundGreen :: TColor
backgroundGreen = TColor $ escapeChar : "[42m"


red :: TColor
red = TColor $ escapeChar : "[31m"


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
applyColor :: TColor -> String -> String
applyColor (TColor color) str = color ++ str ++ reset


numToBlock :: Int -> Char
numToBlock 0 = ' '
numToBlock 1 = oneEigthBlock
numToBlock 2 = threeFourthBlock
numToBlock 3 = fiveEigthBlock
numToBlcok 4 = sevenEigthBlock 
