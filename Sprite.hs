{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Sprite where

import Control.Concurrent.MVar
import Control.Lens
import Control.Lens.TH
import Control.Monad (liftM)
import Attributes


type Position = (Int, Int)


data SpriteType = Fire | Water | Earth | Air


data Sprite = Sprite { _sprName :: String
                     , _sprType :: SpriteType
                     , _sprAttributes :: Attributes
                     }
$(makeLenses ''Sprite)


data Team = GreenTeam | RedTeam


data SpriteContainer_ = SpriteContainer_ { _conPosition :: Maybe Position
                                         , _conSprite :: Sprite
                                         , _conAttributes :: Attributes
                                         , _conTeam :: Team 
                                         }
$(makeLenses ''SpriteContainer_)


data SpriteContainer = SpriteContainer (MVar SpriteContainer_)

-- Query Functions
extractInnerContainer :: SpriteContainer -> MVar SpriteContainer_
extractInnerContainer (SpriteContainer innerContainer) = innerContainer


getContainerPosition :: SpriteContainer -> IO (Maybe Position)
getContainerPosition (SpriteContainer sprCon_) = do
    liftM _conPosition $ readMVar sprCon_


getSpriteAttributes :: SpriteContainer -> IO (Attributes)
getSpriteAttributes (SpriteContainer sprCon_) = do
    liftM (view (conSprite . sprAttributes)) $ readMVar sprCon_


getSpriteAttribute :: SpriteContainer -> Lens' Attributes a -> IO (a)
getSpriteAttribute (SpriteContainer sprCon_) attr = do
    liftM (view (conSprite . sprAttributes . attr)) $ readMVar sprCon_


getContainerAttribute :: SpriteContainer -> Lens' Attributes a -> IO (a)
getContainerAttribute (SpriteContainer sprCon_) attr = do
    liftM (view (conAttributes . attr)) $ readMVar sprCon_


getContainerTeam :: SpriteContainer -> IO Team 
getContainerTeam (SpriteContainer sprCon_) = do
    liftM _conTeam $ readMVar sprCon_


-- Create Functions
newSpriteContainer :: Sprite -> Team -> Maybe Position -> IO SpriteContainer
newSpriteContainer sprite team mpos = newMVar innerContainer >>= return . SpriteContainer
    where innerContainer = SpriteContainer_ { _conPosition=mpos 
                                            , _conSprite=sprite
                                            , _conAttributes=_sprAttributes sprite
                                            , _conTeam=team
                                            }


newGreenSpriteContainer :: Sprite -> Maybe Position -> IO SpriteContainer
newGreenSpriteContainer sprite mpos = newSpriteContainer sprite GreenTeam mpos


newRedSpriteContainer :: Sprite -> Maybe Position -> IO SpriteContainer
newRedSpriteContainer sprite mpos = newSpriteContainer sprite RedTeam mpos


-- Manipulate Functions
-- -- Core Functions
updateContainerPosition :: SpriteContainer -> Position -> IO ()
updateContainerPosition (SpriteContainer sprCon_) pos = do 
    modifyMVar_ sprCon_ (return . over conPosition (const $ Just pos))


modifyAttribute :: SpriteContainer -> Lens' Attributes a -> (a -> a) -> IO ()
modifyAttribute (SpriteContainer sprCon_) lens modification = do
    modifyMVar_ sprCon_ (return . over (conAttributes . lens) modification)


resetAttribute :: Lens' Attributes a -> SpriteContainer -> IO ()
resetAttribute attr (SpriteContainer sprCon_) = do
    baseAttributes <- liftM (view (conSprite . sprAttributes . attr)) $ readMVar sprCon_
    modifyMVar_ sprCon_ (return . over (conAttributes . attr) (const baseAttributes))


-- -- Specific Functions
resetStamina :: SpriteContainer -> IO ()
resetStamina = resetAttribute stamina


resetMovement :: SpriteContainer -> IO ()
resetMovement = resetAttribute speed


subtractMovement :: SpriteContainer -> IO ()
subtractMovement sprCon = modifyAttribute sprCon speed (+ (-Speed 1))


-- Example Sprite
dummy :: Sprite
dummy = Sprite { _sprName="Dummy"
               , _sprType=Fire
               , _sprAttributes=dummyAttributes
               }
    where dummyAttributes = Attributes { _healthPoints=HealthPoints 10
                                       , _speed=Speed 10
                                       , _stamina=Stamina 10
                                       , _physicalAttack=PhysicalAttack 10
                                       , _magicalAttack=MagicalAttack 10
                                       , _physicalDefense=PhysicalDefense 10
                                       , _magicalDefense=MagicalDefense 10
                                       }
