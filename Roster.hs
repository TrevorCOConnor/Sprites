module Roster where

-- Core Haskell
import Control.Lens
import Control.Concurrent.MVar
import Data.List

-- Local Modules
import Ansi
import Attributes

import Sprite

-- Data
data FullRoster = FullRoster [Roster] [Roster]


data Roster = Roster Int Team SpriteContainer


-- Aux Functions
getInitiative :: SpriteContainer -> IO Int
getInitiative sprCon = do
    (Speed spd) <- getContainerAttribute sprCon speed
    return spd


sortOnInitiative :: [SpriteContainer] -> IO [SpriteContainer]
sortOnInitiative sprCons = map fst <$> sortOn snd
                                   <$> zip sprCons
                                   <$> (sequence $ map getInitiative sprCons)
                                   

cycleFullRoster :: FullRoster -> FullRoster
cycleFullRoster (FullRoster passed []) = (FullRoster [] (passed))
cycleFullRoster (FullRoster passed (n:[])) = (FullRoster [] (passed ++ n))
cycleFullRoster (FullRoster passed (n:ns)) = (FullRoster (passed ++ n) ns)


-- Create Functions
createRoster :: Int -> SpriteContainer -> IO Roster
createRoster num sprCon = do 
    team <- getContainerTeam sprCon
    return $ Roster num team sprCon


createFullRoster :: [SpriteContainer] -> IO FullRoster
createFullRoster sprCons = do
    rosters <- sequence $ map (\(num, spr) -> createRoster num spr) $ zip [1..] sprCons
    return $ FullRoster [] rosters


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


displayRoster :: Roster -> IO (String)
displayRoster (Roster _ team sprCon) = do
    let teamColor = teamToColor team
    name <- displaySpriteName teamColor sprCon
    health <- displayContainerHealth teamColor sprCon
    return $ unlines [name, health]


displaySelectedRoster :: Roster -> IO (String)
displaySelectedRoster = fmap (applyColor backgroundWhite) . displayRoster 


displayFullRoster :: FullRoster -> IO (String)
displayFullRoster (FullRoster before (selected:after)) = do
    before' <- sequence $ map displayRoster before
    selected' <- displayRoster selected
    after' <- sequence $ map displayRoster after
    return $ intercalate "\n" $ before' ++ [selected'] ++ after'
