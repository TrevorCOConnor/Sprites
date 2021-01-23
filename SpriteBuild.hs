module SpriteBuild where

import SpriteData
import Actions

data SpriteBase = SpriteBase { baseType        :: SpriteType
                             , baseStats       :: Stats 
                             , moveBase        :: [Action]  
                             , possibleEffects :: [Effect]
                             }

defaultSprite :: String -> SpriteBase -> Sprite
defaultSprite id sb = Sprite { name           = show $ baseType sb 
                             , spriteId       = id
                             , currentHp      = baseHp . baseStats $ sb 
                             , stats          = baseStats sb
                             , currentActions = moveBase sb
                             , innateEffects  = possibleEffects sb
                             , elementType    = Normal
                             , spriteType     = baseType sb
                             , prefixes       = []
                             }

wyvern :: SpriteBase
wyvern = SpriteBase { baseType        = Wyvern
                    , baseStats       = wyvernStats
                    , moveBase        = wyvernMoves
                    , possibleEffects = wyvernEffects
                    }
    where wyvernStats = Stats { baseHp = 10
                              , atk    = 10
                              , magAtk = 10
                              , def    = 10
                              , magDef = 10
                              , spd    = 10
                              , agl    = 10
                              , stm    = 10
                              }
          wyvernMoves   = [bite, fly]
          wyvernEffects = []
