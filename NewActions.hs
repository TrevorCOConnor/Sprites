module NewActions where

import Sprite
import Field
import BattleState


type Effect = SpriteContainer -> BattleState -> Position -> IO ()


data Target = Self
            | Point
            | Sprite 


data NewAction = NewAction { cost :: Cost
                           , target :: Target
                           , effects :: [Effect]
                           }
