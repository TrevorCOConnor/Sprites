module ActionApplications where

import Field
import Sprite
import Attributes
import Actions


-- applyAction :: SpriteContainer -> Action -> Field -> IO ()
-- applyAction sprCon action fld =
--     case actionType of
--       Attack -> attackAction sprCon action fld


-- attackAction :: SpriteContainer -> Action -> Field -> IO ()
-- attackAction sprite action fld = do
--     targets <- chooseEnemyTargets sprite action fld


-- chooseEnemyTargets :: SpriteContainer -> Action -> Field -> IO [Position] 
-- chooseEnemyTargets sprCon action fld = do
--     let numberOfTargets = actionTargetCount action
--     let range = actionRange action
--     origin <- extractContainerPosition sprCon 


applyAttack :: Action -> SpriteContainer -> SpriteContainer -> IO ()
applyAttack attack attacker defender = do
    damage <- calculateDamage (actionDamageType attack) (actionPower attack) attacker defender
    applyDamage defender 


-- Formulas

powerscale :: Float
powerscale = 0.01


calculateDamage :: DamageType -> Power -> SpriteContainer -> SpriteContainer -> IO HealthPoints
calculateDamage dmgtype power attacker defender = 
    case dmgtype of
        PhysicalDamage -> do
            phyAtk <- extractSpriteAttributes attacker >>= return . physicalAttack
            phyDef <- extractSpriteAttributes defender >>= return . physicalDefense
            return $ calculatePhysicalDamage power phyAtk phyDef
        MagicalDamage -> do
            magAtk <- extractSpriteAttributes attacker >>= return . magicalAttack
            magDef <- extractSpriteAttributes defender >>= return . magicalDefense
            return $ calculateMagicalDamage power magAtk magDef


calculatePhysicalDamage :: Power -> PhysicalAttack -> PhysicalDefense -> HealthPoints
calculatePhysicalDamage (Power power) phyAtk phyDef =
    HealthPoints $ round $ (power' * phyAtk') / phyDef' 
    where power' = (fromIntegral power :: Float) * powerscale
          phyAtk' = extractAttributeFloat phyAtk
          phyDef' = extractAttributeFloat phyDef


calculateMagicalDamage :: Power -> MagicalAttack -> MagicalDefense -> HealthPoints
calculateMagicalDamage (Power power) magAtk magDef =
    HealthPoints $ round $ (power' * magAtk') / magDef' 
    where power' = (fromIntegral power :: Float) * powerscale
          magAtk' = extractAttributeFloat magAtk
          magDef' = extractAttributeFloat magDef
