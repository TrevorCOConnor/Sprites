module SpriteCore where

import Data.Maybe

data Sprite = Sprite { sprName     :: Name
                     , sprId       :: Id
                     , currentHp   :: Hp
                     , stats       :: Stats
                     , sprEffects  :: [Effect]
                     , sprActions  :: [Action]
                     , sprElement  :: Element
                     , sprBase     :: SpriteBase
                     , sprPrefixes :: [Prefix]
                     }

data Stats = Stats { baseHp :: Hp
                   , phyAtk :: PhyAtk
                   , magAtk :: MagAtk
                   , phyDef :: PhyDef
                   , magDef :: MagDef
                   , spd    :: Speed
                   , stm    :: Stamina
                   }

data Effect = Effect { fxTargets  :: [Target]
                     , fxFunction :: EffectFunction
                     , fxSprite   :: Maybe BattleSprite
                     }

data Action = Action { actName    :: Name
                     , actTarget  :: TargetType  
                     , actDmg     :: Damage
                     , actDmgType :: DamageType
                     , actEffects :: [Latent Effect]
                     , actCost    :: Energry
                     }

data BattleSprite = BattleSprite { battleSprite   :: Sprite
                                 , battleHp       :: Hp
                                 , battleEffects  :: [Effect]
                                 , modifiers      :: [Modifier] 
                                 , battleElement  :: Element
                                 , battleActions  :: [Action]
                                 , battlePosition :: BattlePosition
                                 , battleTeam     :: Team
                                 }

data Element = Fire | Ice | Lightning | Psychic | Normal
                deriving (Eq, Show)
data Damage     = NoDmg | Dmg Int
data DamageType = NoType | Physical | Magical
                    deriving (Eq, Show)
data TargetType = Self | Selection Int Radius | Emit Ray | Area Shape Origin
data Shape      = Sphere Int | Cube Int
data Ray        = Ray Direction RayType 
data RayType    = Line Int | Cone Int
                    deriving (Eq, Show)
data Direction  = North | East | South | West
                    deriving (Eq, Show)
data Modifier   = Modifier Mod Attr Scalar
data Mod        = Dampen | Enhance
                    deriving (Eq, Show)
data Attr       = PhyAtkAttr | MagAtkAttr | PhyDefAttr | MagDefAttr | SpdAttr | AglAttr
                    deriving (Eq, Show)
data Team       = Team Int
                    deriving (Eq, Show)

newtype Hp      = Hp Int
newtype PhyAtk  = PhyAtk Int
newtype MagAtk  = MagAtk Int
newtype PhyDef  = PhyDef Int
newtype MagDef  = MagDef Int
newtype Speed   = Speed Int
newtype Stamina = Stamina Int
newtype Energy  = Energy Int
newtype LatentEffect = ([Target] -> Effect)

type Name           = String
type Id             = String
type SpriteBase     = String
type Target         = (Int, Int)
type EffectFunction = (Square -> Square)
type Radius         = Int
type Origin         = Int
