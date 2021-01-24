module SpriteBuild where

import Core
import Actions

data SpriteType = Wyvern
                    deriving (Show, Eq)
data SprBase = SprBase { baseType        :: SpriteType
                       , baseStats       :: Stats 
                       , moveBase        :: [Action]  
                       , possibleEffects :: [Effect]
                       }

convertActions :: [Action] -> SpriteActions
convertActions as = fillSprActions as' 
    where as' = (map Just as) ++ replicate (4 - length as) Nothing
          fillSprActions (a:b:c:d:as) = SpriteActions { fstAct = a
                                                      , sndAct = b
                                                      , thdAct = c
                                                      , fthAct = d
                                                      }

defaultSprite :: String -> SprBase -> Sprite
defaultSprite id sb = Sprite { sprName    = show $ baseType sb 
                             , sprId      = id
                             , currentHp  = baseHp . baseStats $ sb 
                             , stats      = baseStats sb
                             , sprActions = convertActions $ moveBase sb
                             , sprEffects = possibleEffects sb
                             , sprElement = Normal
                             , sprBase    = show $ baseType sb
                             , sprPrefix  = Nuetral
                             }

wyvern :: SprBase
wyvern = SprBase { baseType        = Wyvern
                 , baseStats       = wyvernStats
                 , moveBase        = wyvernMoves
                 , possibleEffects = wyvernEffects
                 }
    where wyvernStats = Stats { baseHp = Hp 10
                              , phyAtk = PhyAtk 10
                              , magAtk = MagAtk 10
                              , phyDef = PhyDef 10
                              , magDef = MagDef 10
                              , spd    = Speed 10
                              , stm    = Stamina 10
                              }
          wyvernMoves   = [fly]
          wyvernEffects = []
