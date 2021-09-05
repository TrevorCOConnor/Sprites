module TerminalSelection where


-- Core Haskell
import Data.Maybe
import Text.Read
import Control.Monad (liftM, liftM2)


-- Local Modules
import Sprite
import Square
import Field


validatePositionTarget :: Field -> (Square -> IO Bool) -> Position -> IO Bool
validatePositionTarget fld check pos = do
    let square = getSquare fld pos
    validPosition <- sequence $ liftM check square
    return $ fromMaybe False validPosition


chooseTarget :: IO (Maybe Position)
chooseTarget = do
    input <- getLine
    if input == ""
       then return Nothing
       else do
           let possiblePosition = readMaybe input :: Maybe Position
           if isNothing possiblePosition
              then do
                putStrLn "Incorrect format. Rquired format: (x, y)."
                chooseTarget
              else return $ possiblePosition


chooseMultipleTargets :: Int -> IO [Position]
chooseMultipleTargets numberOfTargets = chooseMultipleTargets_ (reverse [1..numberOfTargets]) []


chooseMultipleTargets_ :: [Int] -> [Position] -> IO [Position]
chooseMultipleTargets_ [] _ = return []
chooseMultipleTargets_ (i:is) previouslySelected = do
    putStrLn $ "Choose target or press <Enter> to submit current selection (" ++ show i ++ " selections remaining):"
    possibleTarget <- chooseTarget
    if isNothing possibleTarget
        then return []
        else do
            let target = fromJust possibleTarget
            if target `elem` previouslySelected 
               then do
                   putStrLn $ "This target has already been selected."
                   chooseMultipleTargets_ (i:is) previouslySelected
               else liftM2 (:) (return target)
                       $ chooseMultipleTargets_ is (target:previouslySelected)
