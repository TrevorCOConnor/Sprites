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
                     , sprPrefixes :: Prefix
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
                     , actEffects :: [LatentEffect]
                     , actCost    :: Energy
                     }

data BattleSprite = BattleSprite { battleSprite   :: Sprite
                                 , battleHp       :: Hp
                                 , battleEffects  :: [Effect]
                                 , modifiers      :: [Modifier] 
                                 , battleElement  :: Element
                                 , battleActions  :: [Action]
                                 , battlePosition :: Position
                                 , battleTeam     :: Team
                                 }

data Square = Square { sqrPosition :: Position
                     , occupant    :: Occupant
                     , sqrEffects  :: [Effect]
                     }

instance Show Square where
    show square =
        case occupant square of
          Vacant          -> if length (sqrEffects square) == 0 then "[ ]" else "[@]"
          ContainSprite _ -> "[S]"
          ContainObject   -> "[#]"

data Occupant = Vacant | ContainSprite BattleSprite | ContainObject

instance Eq Occupant where
    Vacant          == Vacant          = True
    ContainSprite _ == ContainSprite _ = True
    ContainObject   == ContainObject   = True
    _               == _               = False

data Board = Board { bWidth  :: Width
                   , bLength :: Length
                   , rows    :: Rows
                   }

instance Show Board where
    show board = makeColumnHeader (bWidth board) ++ scanRows 1 (rows board)
        where scanRows l []     = []
              scanRows l (r:rs) = '\n':showRow l r ++ scanRows (l+1) rs

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
data Prefix     = Hearty | Strong | Intelligent | Sturdy | Wise | Quick | Resilient

newtype Hp           = Hp Int
newtype PhyAtk       = PhyAtk Int
newtype MagAtk       = MagAtk Int
newtype PhyDef       = PhyDef Int
newtype MagDef       = MagDef Int
newtype Speed        = Speed Int
newtype Stamina      = Stamina Int
newtype Energy       = Energy Int
newtype LatentEffect = LatentEffect ([Target] -> Effect)

type Name           = String
type Id             = String
type SpriteBase     = String
type Target         = (Int, Int)
type EffectFunction = (Square -> Square)
type Radius         = Int
type Origin         = Int
type Scalar         = Float
type Position       = (Int, Int)
type Width          = Int
type Length         = Int
type Rows           = [[Square]]


makeColumnHeader :: Width -> [Char]
makeColumnHeader w = leftBuffer ++ makeColumns [1..w]
    where leftBuffer = replicate 4 ' '
          makeColumns []     = []
          makeColumns (c:cs) = alignLeft 3 (show c) ++ makeColumns cs

showRow :: Int -> [Square] -> [Char]
showRow n sqrs = ' ' : alignLeft 3 (show n) ++ (concat . map show $ sqrs)

alignLeft :: Int -> [Char] -> [Char]
alignLeft l text = text ++ buffer
    where bufferSize = l - length text
          buffer     = replicate bufferSize ' '

alignRight :: Int -> [Char] -> [Char]
alignRight l text = buffer ++ text
    where bufferSize = l - length text
          buffer     = replicate bufferSize ' '

newSquare :: Position -> Square
newSquare p = Square { sqrPosition = p
                     , occupant    = Vacant
                     , sqrEffects  = []
                     }

placeObj :: Square -> Square
placeObj sqr = sqr { occupant = ContainObject }

placeBSprite :: BattleSprite -> Square -> Square
placeBSprite bSprite sqr = sqr { occupant = ContainSprite bSprite }

makeVacant :: Square -> Square
makeVacant sqr = sqr { occupant = Vacant }

placeOccupant :: Occupant -> Square -> Square
placeOccupant o sqr = sqr { occupant = o }

createRow :: Position -> [Square]
createRow (w, l) = map newSquare positions 
    where positions = zip (replicate w l) [1..w]

createBoardRows :: Width -> Length -> Rows
createBoardRows w l = map createRow rowDimensions
    where rowDimensions = zip (replicate l w) [1..l] 

newBoard :: Width -> Length -> Board
newBoard w l = Board w l $ createBoardRows w l

getSquare :: Position -> Board -> Square
getSquare (x, y) board = r !! (x-1)
    where rs = rows board
          r  = rs !! (y-1)

modifySquare :: (Square -> Square) -> Position -> Board -> Board
modifySquare f (x, y) board = board { rows = rs' }
    where rs' = bRows ++ r':aRows
          (bRows, r:aRows) = splitAt (y-1) (rows board)
          (bCols, c:aCols) = splitAt (x-1) r
          c' = f c
          r' = bCols ++ c':aCols

placeBSpriteOnBoard :: BattleSprite -> Position -> Board -> Board
placeBSpriteOnBoard bSprite = modifySquare (placeBSprite bSprite) 

placeObjOnBoard :: Position -> Board -> Board
placeObjOnBoard = modifySquare placeObj

placeObjsOnBoard :: [Position] -> Board -> Board
placeObjsOnBoard ps b = foldr placeObjOnBoard b ps

validPosition :: Position -> Board -> Bool
validPosition (x, y) board = xValid && yValid
    where xValid = 1 <= x && x <= (bWidth board)
          yValid = 1 <= y && y <= (bLength board)

move :: Position -> Position -> Board -> Board
move p1 p2 board =  modifySquare (placeOccupant o) p2 . modifySquare makeVacant p1 $ board 
    where o      = occupant $ getSquare p1 board

moveUp :: Position -> Board -> Board
moveUp p@(x, y) = move p (x, y-1) 

moveDown :: Position -> Board -> Board
moveDown p@(x, y) = move p (x, y+1) 

moveLeft :: Position -> Board -> Board
moveLeft p@(x, y) = move p (x-1, y) 

moveRight :: Position -> Board -> Board
moveRight p@(x, y) = move p (x+1, y) 
