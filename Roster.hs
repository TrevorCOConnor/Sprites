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


displayRoster :: Roster -> IO (String)
displayRoster (Roster _ _ sprCon) = do
    name <- displaySpriteName sprCon
    health <- displayContainerHealth sprCon
    return $ unlines [name, health]
    

displaySelectedRoster :: Roster -> IO (String)
displaySelectedRoster = fmap (applyColor backgroundWhite) . displayRoster 


displayFullRoster :: FullRoster -> IO (String)
displayFullRoster (FullRoster before (selected:after)) = do
    before' <- sequence $ map displayRoster before
    selected' <- displayRoster selected
    after' <- sequence $ map displayRoster after
    return $ intercalate "\n" $ before' ++ [selected'] ++ after'
