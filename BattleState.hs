{-# LANGUAGE RankNTypes #-}

module BattleState where

-- Local Modules
import Sprite
import Field
import Roster


-- Data
data BattleState = BattleState Field FullRoster


-- Aux Functions
cycleBattleStateRoster :: BattleState -> BattleState
cycleBattleStateRoster (BattleState field fullRoster) =
    BattleState field $ cycleFullRoster fullRoster


nextSpriteFromBattleState :: BattleState -> Maybe SpriteContainer
nextSpriteFromBattleState (BattleState _ fullRoster) = do
    (Roster _ _ sprCon) <- nextFromFullRoster fullRoster
    return sprCon


checkWinner :: BattleState -> Maybe Team
checkWinner _ = Nothing
