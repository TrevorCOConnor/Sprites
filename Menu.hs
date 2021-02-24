module Menu where

import Data.List 

sideBySideStrings :: Int -> String -> String -> String
sideBySideStrings width stringA stringB = sideBySideStrings' width as bs
    where as = breakString stringA
          bs = breakString stringB

sideBySideStrings' :: Int -> [String] -> [String] -> String
sideBySideStrings' width as bs = if maxLength < width
                                    then scanLines as bs
                                    else intercalate "\n" as ++ "\n" ++ intercalate "\n" bs
    where maxLength = maximum $ map length as 
          spacer a = replicate (width - length a) ' '
          scanLines []     []     = []
          scanLines as     []     = concat as 
          scanLines []     bs     = spacer "" ++ concat bs
          scanLines (a:as) (b:bs) = (a ++ spacer a ++ b ++ "\n") ++ scanLines as bs

breakString :: String -> [String]
breakString str = reverse $ scanLines str "" []
    where scanLines [] builder lst     = (reverse builder):lst
          scanLines (c:cs) builder lst = 
              if c == '\n'
                then scanLines cs "" $ (reverse builder):lst 
                else scanLines cs (c:builder) lst 

parallelStrings :: Int -> [String] -> String
parallelStrings width strs = scanStrings [] strs
    where scanStrings :: [String] -> [String] -> String 
          scanStrings builder [] = chainStrings adjustedWidth builder
              where adjustedWidth = width `div` (length builder)
          scanStrings builder (a:as) = if parallelFit width (map breakString (a:builder))
                                          then scanStrings (builder ++ [a]) as
                                          else chainStrings adjustedWidth builder
                                                ++ scanStrings [a] as
              where adjustedWidth = width `div` (length builder)

parallelFit :: Int -> [[String]] -> Bool
parallelFit width [] = True
parallelFit width as = width `div` (length as) > maxLength + (length as)
    where maxLength = maximum $ map (maximum . (map length)) as 

chainStrings :: Int -> [String] -> String
chainStrings _     []   = []
chainStrings width strs = foldr (sideBySideStrings width) s ss
    where (s:ss) = reverse strs
