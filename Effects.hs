module Effects where

import SpriteData

addEffect :: Timer Effect -> Sprite
addEffect e bSprite = bSprite { activeEffects = e:currentFx } 
    where currentFx = activeEffects bSprite

applyElement :: Time -> Effect
applyElement time = Effect OnHit function
    where function sprite = addEffect (  

element :: Element -> BattleSprite -> EffectFunction
element Fire attSprite = fireFunction
    where fireFunc defSprite = defSprite { currentHp = newHp }
          dmg   = min 1 ((atk . stats $ attSprite) `div` 10) 
          newHp = (currentHp defSprite) - dmg

