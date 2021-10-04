module Roster where

-- Core Haskell
import Control.Lens
import Control.Concurrent.MVar

-- Local Modules
import Ansi
import Attributes
import Sprite


-- Display Functions
displayHealth :: AnsiColor -> HealthPoints -> HealthPoints -> String
displayHealth color (HealthPoints currentHp) (HealthPoints baseHp) = "HP: " ++ fullBars ++ remBar
    where percentage = (100 * currentHp) `div` baseHp
          percentage25 = percentage `div` 4
          mod5 = percentage `mod` 5
          fullBars = applyColor color $ replicate percentage25 fullBlock
          remBar = applyColor color $ (numToBlock mod5) : []


displayContainerHealth :: AnsiColor -> SpriteContainer -> IO (String)
displayContainerHealth color sprCon = do
    baseHealth <- getSpriteAttribute sprCon healthPoints
    currentHealth <- getContainerAttribute sprCon healthPoints
    return $ displayHealth color currentHealth baseHealth


displaySpriteName :: AnsiColor -> SpriteContainer -> IO (String)
displaySpriteName color (SpriteContainer mvar) = do
    sprCon_ <- readMVar mvar
    return $ "Name: " ++ (applyColor color $ view (conSprite . sprName) sprCon_)


displayContainerRoster :: SpriteContainer -> IO (String)
displayContainerRoster sprCon@(SpriteContainer mvar) = do
    team <- fmap (view conTeam) (readMVar mvar) 
    let teamColor = teamToColor team
    name <- displaySpriteName teamColor sprCon
    health <- displayContainerHealth teamColor sprCon
    return $ unlines [name, health]
