module Roster where

-- Core Haskell
import Control.Lens
import Control.Concurrent.MVar

-- Local Modules
import Ansi
import Attributes
import Sprite


-- Display Functions
displayHealth :: HealthPoints -> HealthPoints -> String
displayHealth (HealthPoints currentHp) (HealthPoints baseHp) = "HP: " ++ fullBars ++ remBar
    where percentage = (100 * currentHp) `div` baseHp
          percentage25 = percentage `div` 4
          mod5 = percentage `mod` 5
          fullBars = applyColor green $ replicate percentage25 fullBlock
          remBar = applyColor green $ (numToBlock mod5) : []


displayContainerHealth :: SpriteContainer -> IO (String)
displayContainerHealth sprCon = do
    baseHealth <- getSpriteAttribute sprCon healthPoints
    currentHealth <- getContainerAttribute sprCon healthPoints
    return $ displayHealth currentHealth baseHealth


displaySpriteName :: SpriteContainer -> IO (String)
displaySpriteName (SpriteContainer sprCon) = do
    sprCon_ <- readMVar sprCon
    return $ "Name: " ++ view (conSprite . sprName) sprCon_


displayContainerTeam :: SpriteContainer -> IO (String)
displayContainerTeam (SpriteContainer sprCon) = do
    sprCon_ <- readMVar sprCon
    return $ "Team: " ++ (show $ view conTeam sprCon_)


displayContainerRoster :: SpriteContainer -> IO (String)
displayContainerRoster sprCon = do
    team <- displayContainerTeam sprCon
    name <- displaySpriteName sprCon
    health <- displayContainerHealth sprCon
    return $ unlines [team, name, health]
