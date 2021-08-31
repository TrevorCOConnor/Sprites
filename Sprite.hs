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


data Team = GreenTeam | RedTeam


data SpriteContainer_ = SpriteContainer_ { sprPosition :: Maybe Position
                                         , sprite :: Sprite
                                         , sprTeam :: Team 
                                         }


data SpriteContainer = SpriteContainer (MVar SpriteContainer_)

-- Query Functions
extractInnerContainer :: SpriteContainer -> MVar SpriteContainer_
extractInnerContainer (SpriteContainer innerContainer) = innerContainer


extractSpritePosition :: SpriteContainer -> IO (Maybe Position)
extractSpritePosition (SpriteContainer innerContainer) = do
    sprCon <- readMVar innerContainer
    return $ sprPosition sprCon


extractSpriteTeam :: SpriteContainer -> IO Team 
extractSpriteTeam (SpriteContainer innerContainer) = do
    sprCon <- readMVar innerContainer
    return $ sprTeam sprCon



-- Manipulate Functions
newSpriteContainer :: Sprite -> Team -> Maybe Position -> IO SpriteContainer
newSpriteContainer sprite team mpos = newMVar innerContainer >>= return . SpriteContainer
    where innerContainer = SpriteContainer_ { sprPosition=mpos 
                                             , sprite=sprite
                                             , sprTeam=team
                                             }


newGreenSpriteContainer :: Sprite -> Maybe Position -> IO SpriteContainer
newGreenSpriteContainer sprite mpos = newSpriteContainer sprite GreenTeam mpos


newRedSpriteContainer :: Sprite -> Maybe Position -> IO SpriteContainer
newRedSpriteContainer sprite mpos = newSpriteContainer sprite RedTeam mpos


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
