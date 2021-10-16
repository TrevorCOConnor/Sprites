module Actions where

import Sprite
import Field


newtype Power = Power Int
newtype ActionRange = ActionRange Int
newtype Cost = Cost Int
newtype TargetCount = TargetCount Int


data DamageType = PhysicalDamage | MagicalDamage


data TargetType = Friendly | Aggressive | Neutral


data Targets = Self 
             | Selection { selectionRange :: ActionRange
                         , selectiontCount :: TargetCount
                         , selectionTargetType :: TargetType
                         }
             | AreaOfEffect { aoeShape :: String
                            , aoeTargetType :: TargetType
                            }


data Target = Self
            | Point
            | SpriteSelection


data MovementType = Line | Fly | Teleport


data Action = AttackAction Attack
            | MovementAction Movement
            | PassiveAction Passive


data Attack = Attack { attackPower :: Power
                     , attackType :: DamageType
                     , attackTargets :: Targets
                     , attackCost :: Cost
                     , attackEffects :: [Effect]
                     }


data Movement = Movement { movementRange :: ActionRange
                         , movementType :: MovementType
                         , movementCost :: Cost
                         , movementTargets :: Targets
                         , movementEffects :: [Effect]
                         }


data Passive = Passive { passiveRange :: ActionRange
                       , passiveCost :: Cost
                       , passiveTargets :: Targets
                       , passiveEffects :: [Effect]
                       }



data Effect = Effect { effectAction :: Action
                     , effectConditions :: [Condition]
                     }


data Condition = Condition

-- data Condition = TargetCondition (SpriteContainer -> IO Bool)
--                | LogCondition (IO Bool) 
--                | SelfCondition (SpriteContainer -> IO Bool)
--                | FieldCondition (Field -> IO Bool)
--                | SquareCondition (Square -> IO Bool) 
