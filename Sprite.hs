module Sprite where

import Control.Concurrent.MVar


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


data SpriteContainer_ = SpriteContainer_ { sprPosition :: Maybe Position
                                         , sprite :: Sprite
                                         }


data SpriteContainer = SpriteContainer (MVar SpriteContainer_)

-- Query Functions
extractInnerContainer :: SpriteContainer -> MVar SpriteContainer_
extractInnerContainer (SpriteContainer innerContainer) = innerContainer


extractSpritePosition :: SpriteContainer -> IO (Maybe Position)
extractSpritePosition (SpriteContainer innerContainer) = do
    sprCon <- readMVar innerContainer
    return $ sprPosition sprCon


-- Manipulate Functions
newSpriteContainer :: Sprite -> Maybe Position -> IO SpriteContainer
newSpriteContainer sprite mpos = newMVar innerContainer >>= return . SpriteContainer
    where innerContainer = SpriteContainer_ { sprPosition=mpos 
                                             , sprite=sprite
                                             }


updateSpritePosition :: SpriteContainer -> Position -> IO ()
updateSpritePosition (SpriteContainer innerContainer) pos = 
    modifyMVar_ innerContainer (return . updateSpritePosition_ pos)


updateSpritePosition_ :: Position -> SpriteContainer_ -> SpriteContainer_
updateSpritePosition_ pos sprCon = sprCon {sprPosition=(Just pos)}



-- Example Sprite
dummy :: Sprite
dummy = Sprite { sprName="Dummy"
               , sprType=Fire
               , sprAttributes=dummyAttributes
               }
    where dummyAttributes = Attributes { healthPoints=HealthPoints 10
                                       , speed=Speed 10
                                       , endurance=Endurance 10
                                       , phyiscalAttack=PhysicalAttack 10
                                       , magicalAttack=MagicalAttack 10
                                       , physicalDefense=PhysicalDefense 10
                                       , magicalDefense=MagicalDefense 10
                                       }
