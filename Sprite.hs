module Sprite where


type Position = (Int, Int)


data SpriteType = Fire | Water | Earth | Air


newtype HealthPoints = HealthPoints Int
newtype Speed = Speed Int
newtype Endurance = Endurance Int
newtype PhysicalAttack = PhysicalAttack Int
newtype MagicalAttack = MagicalAttack Int
newtype PhysicalDefense = PhysicalDefense Int
newtype MagicalDefense = MagicalDefense Int


data Attributes = Attributes { healthPoints :: HealthPoints
                             , speed :: Speed
                             , endurance :: Endurance
                             , phyiscalAttack :: PhysicalAttack
                             , magicalAttack :: MagicalAttack
                             , physicalDefense :: PhysicalDefense
                             , magicalDefense :: MagicalDefense
                             }


data Sprite = Sprite { sprName :: String
                     , sprType :: SpriteType
                     , sprAttributes :: Attributes
                     }


data SpriteContainer = SpriteContainer { sprPosition :: Position
                                       , sprite :: String
                                       }
