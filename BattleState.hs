{-# LANGUAGE RankNTypes #-}

module BattleState where

-- Local Modules
import Field
import Roster


-- Data
data BattleState = BattleState Field FullRoster
