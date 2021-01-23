module SpriteData where

import Data.Maybe 

data Sprite = Sprite { name           :: Name
                     , spriteId       :: Id
                     , currentHp      :: Hp
                     , stats          :: Stats
                     , innateEffects  :: [Effect] 
                     , currentActions :: [Action]
                     , elementType    :: Element
                     , spriteType     :: SpriteType
                     , prefixes       :: [Prefixes]
                     }

type Id             = [Char]
type Name           = [Char]

data Stats = Stats { baseHp :: Hp
                   , atk    :: Attack
                   , magAtk :: MagicalAttack
                   , def    :: Defense
                   , magDef :: MagicalDefense
                   , spd    :: Speed
                   , agl    :: Agility
                   , stm    :: Stamina
                   }
data Element    = Fire | Ice | Lightning | Psychic | Normal
                    deriving (Eq, Show)
data SpriteType = Dragon | Basic | Wyvern
                    deriving (Eq, Show)
data Prefixes   = Quick
                    deriving (Eq, Show)

type Hp             = Int
type Attack         = Int
type MagicalAttack  = Int
type Defense        = Int
type MagicalDefense = Int
type Speed          = Int
type Agility        = Int
type Stamina        = Int

data Action = Action { actName    :: Name 
                     , actTarget  :: Target
                     , actDamage  :: Damage
                     , actDmgType :: DamageType
                     , actEffects :: [Effect]
                     , actCost    :: Energy
                     }

type Energy = Int

data DamageType = NoType | Physical | Magical
data Damage = NoDmg | Damage Int
data Target = Self | Selection Int Radius | Area Shape Origin 
data Shape = Line Int | Cone Int | Sphere Int | Cube Int

type Scalar         = Float
type Radius         = Int
type Origin         = Int

data BattleSprite = BattleSprite { sprite         :: Sprite
                                 , battleHp       :: Hp
                                 , activeEffects  :: [Effect]
                                 , modifiers      :: [Modifier] 
                                 , battleElement  :: Element
                                 , battleActions  :: [Action]
                                 , battlePosition :: BattlePosition
                                 , battleTeam     :: Team
                                 }

type BattlePosition = (Int, Int)

toBattle :: Team -> BattlePosition -> Sprite -> BattleSprite
toBattle team position sprite = BattleSprite { sprite         = sprite
                                             , battleHp       = currentHp sprite
                                             , activeEffects  = []
                                             , modifiers      = []
                                             , battleElement  = elementType sprite
                                             , battleActions  = currentActions sprite
                                             , battlePosition = position
                                             , battleTeam     = team
                                             }

fromBattle :: BattleSprite -> Sprite
fromBattle bSprite = s { currentHp = newHp }
    where newHp = battleHp bSprite
          s     = sprite bSprite


data Team       = Black | White
                    deriving (Eq, Show)
data Modifier   = Modifier Mod Attr Scalar
data Mod        = Dampen | Enhance
data Attr       = Atk | MagAtk | Def | MagDef | Spd | Agl
data Timer a    = Timer Duration a
data StateName  = MatchStart | DamageTaken | OnHit
                    deriving (Eq)
data State a    = State StateName a
data Duration   = Cont | Duration Int

newtype Effect   = Effect (State EffectFunction)
newtype Location = Location (Int, Int)
                    deriving (Eq, Show)

type EffectFunction = (BattleSprite -> [BattleSprite] -> BattleSprite)

toEffect :: StateName -> EffectFunction -> Effect
toEffect a f = Effect (State a f)

fromEffect :: Effect -> State EffectFunction
fromEffect (Effect a) = a

contTimer :: a -> Timer a
contTimer obj = Timer Cont obj

toTimer :: Int -> a -> Timer a
toTimer time obj = Timer (Duration time) obj

moveTimer :: Timer a -> Maybe (Timer a)
moveTimer t@(Timer Cont _) = Just t
moveTimer (Timer (Duration n) obj) | n <= 0    = Nothing
                                   | otherwise = Just $ toTimer (n-1) obj 

moveTimers :: [Timer a] -> [Timer a]
moveTimers []     = []
moveTimers (t:ts) = if isNothing movedT 
                       then moveTimers ts 
                       else fromJust movedT : moveTimers ts
    where movedT = moveTimer t

toState :: StateName -> a -> State a
toState s obj = State s obj

stateActivation :: StateName -> [State a] -> [a]
stateActivation state list = foldr f [] list 
    where f (State s obj) objs = if s == state then obj:objs else objs
