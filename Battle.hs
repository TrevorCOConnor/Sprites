{-# LANGUAGE RankNTypes #-}

module Battle where

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
import Roster


-- Data
data BattleState = BattleState Field FullRoster


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
movementAction :: BattleState -> SpriteContainer -> IO ()
movementAction bstate@(BattleState field fRoster) sprCon = do
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
               movementAction bstate sprCon


spriteAction :: BattleState -> SpriteContainer -> IO ()
spriteAction bstate sprCon = do
    putStrLn "Feature not currently supported."
    threadDelay 2000000


-- Aux Functions
handleTurnAction :: BattleState -> SpriteContainer -> TurnAction -> IO()
handleTurnAction bstate@(BattleState field fRoster) sprCon turnaction = do 
    case turnaction of
        Movement -> do
            movementAction bstate sprCon
            turnIO bstate sprCon
        SpriteAction -> do
            spriteAction bstate sprCon
            turnIO bstate sprCon
        EndTurn -> return () 


checkAttributeEqual :: (Eq a) => SpriteContainer -> Lens' Attributes a -> a -> IO Bool
checkAttributeEqual sprCon attr value = (== value) `fmap` getContainerAttribute sprCon attr


updateTurnMap :: SpriteContainer -> IO (CommandMap TurnAction)
updateTurnMap sprCon = do
    noMovement <- checkAttributeEqual sprCon speed (Speed 0)
    noStamina <- checkAttributeEqual sprCon stamina (Stamina 0)
    let keysToRemove = map snd $ filter fst $ zip [noMovement, noStamina] ['m', 'a']
    return $ foldr M.delete turnMap keysToRemove


-- Defaults
fieldSize :: Postition
fieldSize = (30, 30)


-- Main
battle :: [Sprite] -> [Sprite] -> IO ()
battle greens reds = do
    battleState <- beginBattle greens reds
    turnIO battleState


beginBattle :: [Sprite] -> [Sprite] -> IO BattleState
beginBattle greens reds = do
    field <- newField fieldSize
        greenSprites <- sequence $ map (\spr loc -> placeGreenSprite field pos spr)
                             $ zip greens $ greenStartingPositions size
    redSprites <- sequence $ map (\spr loc -> placeRedSprite field pos spr)
                             $ zip reds $ redStartingPositions size
    let greenSprites' = map fromJust $ filter isJust greenSprites
    let redSprites' = map fromJust $ filter isJust redSprites
    fullRoster <- createFullRoster (greenSprites' ++ redSprites')
    return (BattleState field fullRoster)


turnIO :: BattleState -> SpriteContainer -> IO ()
turnIO bstate@(BattleState field fRoster) sprCon = do
    displayField field
    newTurnMap <- updateTurnMap sprCon
    let pairs = M.assocs newTurnMap
    let displayPairs = concat [(show action) ++ " (" ++ [key] ++ ") " | (key, action) <- pairs]
    putStrLn $ "Make selection: " ++ displayPairs
    command <- getCommand turnMap
    putStrLn $ show command
    handleTurnAction bstate sprCon command
