module Actions where

import SpriteData

fly :: Action
fly = Action { actName    = "Fly"
             , actTarget  = Self
             , actDamage  = NoDmg
             , actDmgType = NoType 
             , actEffects = []
             , actCost    = 3
             }

bite :: Action 
bite = Action { actName    = "Bite"
              , actTarget  = Selection 1 1
              , actDamage  = Damage 5
              , actDmgType = Physical
              , actEffects = []
              , actCost    = 3
              }

spray :: Action
spray = Action { actName    = "Spray"
               , actTarget  = Area (Cone 4) 0
               , actDamage  = Damage 4 
               , actDmgType = Magical
               , actEffects = []
               , actCost    = 10
               }
