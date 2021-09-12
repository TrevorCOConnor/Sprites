{-# LANGUAGE RankNTypes #-}

module BattleState where

-- Core Haskell
import qualified Data.Map as M
import Data.Maybe
import Control.Lens
import Control.Concurrent

-- Local Modules
import Command
import Field
import TerminalDisplay
import Sprite
import Attributes


-- Data

data TurnAction = Movement | SpriteAction | EndTurn
    deriving (Show)


-- Command Maps

movementMap :: CommandMap (Position -> Position)
movementMap = M.fromList [ ('w', up)
                         , ('a', left)
                         , ('s', down)
                         , ('d', right)
                         ]


turnMap :: CommandMap TurnAction
turnMap = M.fromList [ ('m', Movement)
                     , ('a', SpriteAction)
                     , ('e', EndTurn)
                     ]


-- Turn Actions

movementAction :: Field -> SpriteContainer -> IO ()
movementAction field sprCon = do
    remainingMovement <- getContainerAttribute sprCon speed
    if remainingMovement <= (Speed 0)
       then return ()
       else do
        displayField field
        putStrLn $ "Remaining movement: " ++ (show remainingMovement)
        movement <- getCommandMaybe movementMap
        if isNothing movement
           then return ()
           else do
               let movement' = fromJust movement
               moved <- moveSprite field movement' sprCon
               if moved
                  then subtractMovement sprCon
                  else return ()
               movementAction field sprCon


spriteAction :: Field -> SpriteContainer -> IO ()
spriteAction field sprCon = do
    putStrLn "Feature not currently supported."
    threadDelay 2000000


-- Aux Functions

handleTurnAction :: Field -> SpriteContainer -> TurnAction -> IO()
handleTurnAction field sprCon turnaction = do 
    case turnaction of
        Movement -> do
            movementAction field sprCon
            turnIO field sprCon
        SpriteAction -> do
            spriteAction field sprCon
            turnIO field sprCon
        EndTurn -> return () 


checkAttributeEqual :: (Eq a) => SpriteContainer -> Lens' Attributes a -> a -> IO Bool
checkAttributeEqual sprCon attr value = (== value) `fmap` getContainerAttribute sprCon attr


updateTurnMap :: SpriteContainer -> IO (CommandMap TurnAction)
updateTurnMap sprCon = do
    noMovement <- checkAttributeEqual sprCon speed (Speed 0)
    noStamina <- checkAttributeEqual sprCon stamina (Stamina 0)
    let keysToRemove = map snd $ filter fst $ zip [noMovement, noStamina] ['m', 'a']
    return $ foldr M.delete turnMap keysToRemove


-- Main

turnIO :: Field -> SpriteContainer -> IO ()
turnIO field sprCon = do
    displayField field
    newTurnMap <- updateTurnMap sprCon
    let pairs = M.assocs newTurnMap
    let displayPairs = concat [(show action) ++ " (" ++ [key] ++ ") " | (key, action) <- pairs]
    putStrLn $ "Make selection: " ++ displayPairs
    command <- getCommand turnMap
    putStrLn $ show command
    handleTurnAction field sprCon command
