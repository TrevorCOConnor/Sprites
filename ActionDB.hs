fly :: Action
fly = Action { actName    = "Fly"
             , actTarget  = Selection Unoccupied 1 10
             , actDmg     = NoDmg
             , actDmgType = NoType 
             , actEffects = [freeMove]
             , actCost    = Energy 3
             }

spray :: Action
spray = Action { actName    = "Spray"
               , actTarget  = Emit 4 Cone
               , actDmg     = Dmg 4 
               , actDmgType = Magical
               , actEffects = []
               , actCost    = Energy 10
               }

-- bite :: Action 
-- bite = Action { actName    = "Bite"
--               , actTarget  = Selection 1 1
--               , actDamage  = Damage 5
--               , actDmgType = Physical
--               , actEffects = []
--               , actCost    = 3
--               }
-- 

