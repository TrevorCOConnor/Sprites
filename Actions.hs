module Actions where

import Core
import Text.Read
import Shapes

dist :: Position -> Position -> Int
dist (a, b) (x, y) = round $ sqrt ( radicand )
    where radicand = fromIntegral $ (x-a)^2 + (y-b)^2

isTarget :: [Char] -> Bool
isTarget txt = 
    case r of 
      Nothing -> False
      Just _  -> True
    where r = readMaybe txt :: Maybe (Int, Int)

correctVacancy :: Vacancy -> Square -> Bool
correctVacancy vac sqr = compareVacancy vac (occupant sqr)
    where compareVacancy Occupied   (ContainSprite _) = True
          compareVacancy Occupied   _                 = False
          compareVacancy Unoccupied Vacant            = True
          compareVacancy Unoccupied _                 = False

validTarget :: Position -> TargetType -> Square -> Bool
validTarget (x, y) (Emit _) sqr = x == a || y == b 
    where (a, b) = sqrPosition sqr
validTarget origin (Selection v n r) sqr = withinRange && rightVacancy
    where p2           = sqrPosition sqr
          withinRange  = dist origin p2 < r
          rightVacancy = correctVacancy v sqr

paintValidTargets :: BattleSprite -> Action -> Board -> Board
paintValidTargets bSprite action board = board { rows = rs'}
    where tType = actTarget action 
          rs  = rows board 
          rs' = map (map f) rs
          valid = validTarget (battlePosition bSprite) tType
          f sqr = if valid sqr then markSquare sqr else sqr

selectTarget :: IO Target
selectTarget = do
    s <- getLine
    if isTarget s
       then return (read s :: (Int, Int))
       else selectTarget

selectAndValidateTarget :: BattleSprite -> TargetType -> Board -> IO Target
selectAndValidateTarget bSprite tType board = do
    t <- selectTarget
    let sqr = getSquare t board
    let origin = (battlePosition bSprite)
    if validTarget origin tType sqr
       then return t
       else selectAndValidateTarget bSprite tType board

generateTargets :: BattleSprite -> TargetType -> Board -> [Target]
generateTargets bSprite Self _ = [battlePosition bSprite]

chooseTargets :: BattleSprite -> TargetType -> Board -> IO [Target]
chooseTargets bSprite Self         board = return []
chooseTargets bSprite t@(Emit _)   board = fmap (:[]) (selectAndValidateTarget bSprite t board)
chooseTargets bSprite (Area _ 0)   board = return []
chooseTargets bSprite t@(Area _ n) board = fmap (:[]) (selectAndValidateTarget bSprite t board)
chooseTargets bSprite t@(Selection v 0 r) board = return []
chooseTargets bSprite t@(Selection v n r) board = do
    t  <- selectAndValidateTarget bSprite t board
    ts <- chooseTargets bSprite (Selection v (n-1) r) board
    return (t:ts)
    
useAction :: (SpriteActions -> Maybe Action) -> BattleSprite -> Board -> IO (BattleSprite, Board)
useAction f bSprite board = do
    let Just action  = f $ battleActions bSprite
    let tType   = actTarget action
    let effects = actEffects action
    putStr . show $ paintValidTargets bSprite action board
    targets <- chooseTargets bSprite tType board
    let f eff = eff targets bSprite
    return $ (bSprite, foldr f board effects)

fly :: Action
fly = Action { actName    = "Fly"
             , actTarget  = Selection Unoccupied 1 10
             , actDmg     = NoDmg
             , actDmgType = NoType 
             , actEffects = [freeMove]
             , actCost    = Energy 3
             }

freeMove :: [Target] -> BattleSprite -> Board -> Board
freeMove (t:[]) bSprite board = move origin t board
    where origin = battlePosition bSprite


-- bite :: Action 
-- bite = Action { actName    = "Bite"
--               , actTarget  = Selection 1 1
--               , actDamage  = Damage 5
--               , actDmgType = Physical
--               , actEffects = []
--               , actCost    = 3
--               }
-- 
-- spray :: Action
-- spray = Action { actName    = "Spray"
--                , actTarget  = Area (Cone 4) 0
--                , actDamage  = Damage 4 
--                , actDmgType = Magical
--                , actEffects = []
--                , actCost    = 10
--                }
