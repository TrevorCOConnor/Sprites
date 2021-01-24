module Actions where

import Core
import Text.Read

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

showValidTargets :: BattleSprite -> Action -> Board -> Board
showValidTargets bSprite action board = board { rows = rs'}
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
    
chooseTargets :: TargetType -> IO [Target]
chooseTargets Self = return []
chooseTargets (Emit _) = fmap (:[]) selectTarget 
chooseTargets (Area _ 0) = return []
chooseTargets (Area _ n) = fmap (:[]) selectTarget
chooseTargets (Selection v 0 r) = do return [] 
chooseTargets (Selection v n r) = do 
    t <- selectTarget
    ts <- chooseTargets (Selection v (n-1) r)
    return (t : ts)

getAndDisplayTargets :: TargetType -> IO ()
getAndDisplayTargets tType = do
    targets <- chooseTargets tType
    foldr (\x y -> do (putStrLn . show) x; y) (putStr "") targets

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
