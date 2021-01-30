module Core where

import Data.Maybe

data Sprite = Sprite { sprName    :: Name
                     , sprId      :: Id
                     , currentHp  :: Hp
                     , stats      :: Stats
                     , sprEffects :: [Effect]
                     , sprActions :: SpriteActions
                     , sprElement :: Element
                     , sprBase    :: SpriteBase
                     , sprPrefix  :: Prefix
                     }

instance Show Sprite where
    show spr = sprName spr

data SpriteActions = SpriteActions { fstAct :: Maybe Action
                                   , sndAct :: Maybe Action
                                   , thdAct :: Maybe Action
                                   , fthAct :: Maybe Action
                                   }

data Stats = Stats { baseHp :: Hp
                   , phyAtk :: PhyAtk
                   , magAtk :: MagAtk
                   , phyDef :: PhyDef
                   , magDef :: MagDef
                   , spd    :: Speed
                   , stm    :: Stamina
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
                                 , battleActions  :: SpriteActions
                                 , battlePosition :: Position
                                 , battleTeam     :: Team
                                 }

instance Show BattleSprite where
    show bSprite = show . battleSprite $ bSprite

data Square = Square { sqrPosition :: Position
                     , occupant    :: Occupant
                     , sqrEffects  :: [Effect]
                     , marked      :: Bool
                     }

instance Show Square where
    show square =
        case occupant square of
          Vacant          | marked square                  -> "[+]"
                          | length (sqrEffects square) > 0 -> "[@]"
                          | otherwise                      -> "[ ]"
          ContainSprite _ | marked square                  -> "[x]"
                          | otherwise                      -> "[S]"
          ContainObject                                    -> "[#]"

data Occupant = Vacant | ContainSprite BattleSprite | ContainObject
                deriving (Show)

instance Eq Occupant where
    Vacant          == Vacant          = True
    ContainSprite _ == ContainSprite _ = True
    ContainObject   == ContainObject   = True
    _               == _               = False

data Board = Board { bWidth  :: Width
                   , bLength :: Length , rows    :: Rows
                   }

instance Show Board where
    show board = makeColumnHeader (bWidth board) ++ scanRows 1 (rows board)
        where scanRows l []     = []
              scanRows l (r:rs) = '\n':showRow l r ++ scanRows (l+1) rs
data Element = Fire | Ice | Lightning | Psychic | Poison | Normal
                deriving (Eq, Show)
data Damage     = NoDmg | Dmg Int
data DamageType = NoType | Physical | Magical
                    deriving (Eq, Show)
data TargetType = Self | Selection Vacancy Choices Radius | Emit Ray | Area Shape Origin
data Vacancy    = Occupied | Unoccupied | Indifferent
data Shape      = Sphere Int | Cube Int
data Ray        = Line Int | Cone Int
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
data Prefix     = Nuetral | Hearty | Strong | Intelligent | Sturdy | Wise | Quick | Resilient

newtype Hp           = Hp Int
newtype PhyAtk       = PhyAtk Int
newtype MagAtk       = MagAtk Int
newtype PhyDef       = PhyDef Int
newtype MagDef       = MagDef Int
newtype Speed        = Speed Int
newtype Stamina      = Stamina Int
newtype Energy       = Energy Int

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
type LatentEffect   = [Target] -> BattleSprite -> Board -> Board
type Effect         = BattleSprite -> Board -> Board
type Choices        = Int

toBattle :: Team -> Position -> Sprite -> BattleSprite
toBattle team position spr = BattleSprite { battleSprite   = spr
                                          , battleHp       = currentHp spr
                                          , battleEffects  = []
                                          , modifiers      = []
                                          , battleElement  = sprElement spr
                                          , battleActions  = sprActions spr
                                          , battlePosition = position
                                          , battleTeam     = team
                                          }

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
                     , marked      = False
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

markSquare :: Square -> Square
markSquare sqr = sqr { marked = True }

markSquaresOnBoard :: [Position] -> Board -> Board
markSquaresOnBoard (p:ps) board = modifySquare markSquare p $ markSquaresOnBoard ps board
markSquaresOnBoard []     board = board

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

placeSpriteOnBoard :: Team -> Position -> Sprite -> Board -> Board
placeSpriteOnBoard team position spr = modifySquare (placeBSprite bSprite) position
    where bSprite = toBattle team position spr

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

moveBSpriteUp :: (BattleSprite, Board) -> (BattleSprite, Board)
moveBSpriteUp (bSprite, board) = (bSprite { battlePosition = p2}, move p1 p2 board)
    where p1@(x, y) = battlePosition bSprite
          p2        = (x, y-1)

moveBSpriteDown :: (BattleSprite, Board) -> (BattleSprite, Board)
moveBSpriteDown (bSprite, board) = (bSprite { battlePosition = p2}, move p1 p2 board)
    where p1@(x, y) = battlePosition bSprite
          p2        = (x, y+1)

moveBSpriteLeft :: (BattleSprite, Board) -> (BattleSprite, Board)
moveBSpriteLeft (bSprite, board) = (bSprite { battlePosition = p2}, move p1 p2 board)
    where p1@(x, y) = battlePosition bSprite
          p2        = (x-1, y)

moveBSpriteRight :: (BattleSprite, Board) -> (BattleSprite, Board)
moveBSpriteRight (bSprite, board) = (bSprite { battlePosition = p2}, move p1 p2 board)
    where p1@(x, y) = battlePosition bSprite
          p2        = (x+1, y)

