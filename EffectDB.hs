-- LatentEffect = [Target] -> BattleSprite -> Board -> Board
--
freeMove :: LatentEffect
freeMove (t:[]) bSprite board = move origin t board
    where origin = battlePosition bSprite
