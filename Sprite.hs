module Sprite where

import Control.Concurrent.MVar
import Attributes


type Position = (Int, Int)


data SpriteType = Fire | Water | Earth | Air


data Sprite = Sprite { sprName :: String
                     , sprType :: SpriteType
                     , sprAttributes :: Attributes
                     }


data Team = GreenTeam | RedTeam


data SpriteContainer_ = SpriteContainer_ { conPosition :: Maybe Position
                                         , conSprite :: Sprite
                                         , conAttributes :: Attributes
                                         , conTeam :: Team 
                                         }


data SpriteContainer = SpriteContainer (MVar SpriteContainer_)

-- Query Functions
extractInnerContainer :: SpriteContainer -> MVar SpriteContainer_
extractInnerContainer (SpriteContainer innerContainer) = innerContainer


extractContainerPosition :: SpriteContainer -> IO (Maybe Position)
extractContainerPosition sprCon = do
    sprCon_ <- readMVar $ extractInnerContainer sprCon
    return $ conPosition sprCon_


extractSpriteAttributes :: SpriteContainer -> IO (Attributes)
extractSpriteAttributes sprCon = do
    sprCon_ <- readMVar $ extractInnerContainer sprCon
    let sprite = conSprite sprCon_
    return $ sprAttributes sprite


extractSpriteTeam :: SpriteContainer -> IO Team 
extractSpriteTeam (SpriteContainer innerContainer) = do
    sprCon <- readMVar innerContainer
    return $ conTeam sprCon


-- Create Functions
newSpriteContainer :: Sprite -> Team -> Maybe Position -> IO SpriteContainer
newSpriteContainer sprite team mpos = newMVar innerContainer >>= return . SpriteContainer
    where innerContainer = SpriteContainer_ { conPosition=mpos 
                                            , conSprite=sprite
                                            , conAttributes=sprAttributes sprite
                                            , conTeam=team
                                            }


newGreenSpriteContainer :: Sprite -> Maybe Position -> IO SpriteContainer
newGreenSpriteContainer sprite mpos = newSpriteContainer sprite GreenTeam mpos


newRedSpriteContainer :: Sprite -> Maybe Position -> IO SpriteContainer
newRedSpriteContainer sprite mpos = newSpriteContainer sprite RedTeam mpos


-- Manipulate Functions
updateSpritePosition :: SpriteContainer -> Position -> IO ()
updateSpritePosition sprCon pos = do 
    let sprCon_ = extractInnerContainer sprCon
    modifyMVar_ sprCon_ (\con -> return $ con {conPosition=(Just pos)})


updateSpriteAttribute :: SpriteContainer -> (Attributes -> Attributes) -> IO ()
updateSpriteAttribute sprCon func = do
    oldAttributes <- extractSpriteAttributes sprCon
    let newAttributes = func oldAttributes
    let sprCon_ = extractInnerContainer sprCon
    modifyMVar_ sprCon_ (\con -> return $ con{conAttributes=newAttributes})


modifyAttribute :: (AttributeValue a) => SpriteContainer -> (a -> a) -> IO ()
modifyAttribute sprCon  = do
    spriteAttributes <- extractSpriteAttributes sprCon 
    let currentAttribute =  spriteAttributes
    let newHealth = applyToAttributes (-) currentHealth damage


resetStamina :: SpriteContainer -> IO ()
resetStamina sprCon = do
    baseAttributes <- extractSpriteAttributes sprCon
    let baseStamina = stamina baseAttributes
    let mvar = extractInnerContainer sprCon
    sprCon_ <- readMVar mvar
    let newAttributes = updateAttribute baseStamina $ conAttributes sprCon_
    modifyMVar_ mvar (\x -> return x {conAttributes=newAttributes})



-- Example Sprite
dummy :: Sprite
dummy = Sprite { sprName="Dummy"
               , sprType=Fire
               , sprAttributes=dummyAttributes
               }
    where dummyAttributes = Attributes { healthPoints=HealthPoints 10
                                       , speed=Speed 10
                                       , stamina=Stamina 10
                                       , physicalAttack=PhysicalAttack 10
                                       , magicalAttack=MagicalAttack 10
                                       , physicalDefense=PhysicalDefense 10
                                       , magicalDefense=MagicalDefense 10
                                       }
