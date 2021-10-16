module ActionApplications where

import BattleState
import Field
import Sprite
import Attributes
import Actions


applyAction :: SpriteContainer -> Action -> BattleState -> IO ()
applyAction sprCon action bstate =
    case actionType of
      Attack -> applyAttack action sprCon bstate
      Movement -> applyMovement action sprCon bstate


applyAttack :: Action -> SpriteContainer -> SpriteContainer -> IO ()
applyAttack attack attacker defender = do
    damage <- calculateDamage (actionDamageType attack) (actionPower attack) attacker defender
    effects <- applyEffects (actionEffects attack)
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
