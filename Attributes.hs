{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Attributes where

import Control.Lens hiding (element)
import Control.Lens.TH


-- Definitions
newtype HealthPoints = HealthPoints Int
    deriving (Num, Show, Eq, Ord)
newtype Speed = Speed Int
    deriving (Num, Show, Eq, Ord)
newtype Stamina = Stamina Int
    deriving (Num, Show, Eq, Ord)
newtype PhysicalAttack = PhysicalAttack Int
    deriving (Num, Show, Eq, Ord)
newtype MagicalAttack = MagicalAttack Int
    deriving (Num, Show, Eq, Ord)
newtype PhysicalDefense = PhysicalDefense Int
    deriving (Num, Show, Eq, Ord)
newtype MagicalDefense = MagicalDefense Int
    deriving (Num, Show, Eq, Ord)


data Attributes = Attributes { _healthPoints :: HealthPoints
                             , _speed :: Speed
                             , _stamina :: Stamina
                             , _physicalAttack :: PhysicalAttack
                             , _magicalAttack :: MagicalAttack
                             , _physicalDefense :: PhysicalDefense
                             , _magicalDefense :: MagicalDefense
                             }


$(makeLenses ''Attributes)
