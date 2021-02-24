import Core
import SpriteBuild
import Menu 

data Roster = Roster { teamName :: Name
                     , team :: Team
                     , fstSprite :: BattleSprite
                     , sndSprite :: BattleSprite
                     , thrSprite :: BattleSprite
                     }

-- healthBar :: BattleSprite -> String
-- healthBar bSprite = "[" ++ replicate percentage '\\' ++ replicate percentageInv ' ' ++ "]"
--     where health = fromIntegral (battleHp bSprite) :: Float
--           maxHealth = fromIntegral (baseHp . stats . battleSprite $ bSprite) :: Float
--           percentage = (round (health / maxHealth) * 100) `div` scale
--           scale = 5
--           percentageInv = (100 `div` scale) - percentage 

healthBar :: BattleSprite -> String
healthBar bSprite = color ++ replicate percentage ' ' ++ "\x1b[0m"
    where health = fromIntegral (battleHp bSprite) :: Float
          maxHealth = fromIntegral (baseHp . stats . battleSprite $ bSprite) :: Float
          percentage = (round (health / maxHealth) * 100) `div` scale
          scale = 5
          color | percentage < (25 `div` scale) = red
                | percentage < (50 `div` scale) = yellow
                | otherwise                     = green
          red = "\x1b[41m"
          yellow = "\x1b[43m" 
          green = "\x1b[42m"

battleSpriteStats :: Roster -> (Roster -> BattleSprite) -> [Char]
battleSpriteStats roster f =  tab ++ (sprName . battleSprite . f) roster ++ "\n"
                                   ++ tab ++ "Hp: " ++ (healthBar . f) roster ++ "\n" 
                                   ++ tab ++ "Type: " ++ (sprBase . battleSprite . f) roster ++ "\n"
                                   ++ tab ++ "Element: " ++ (show . battleElement . f) roster ++ "\n"
    where tab = "  "
instance Show Roster where
    show roster = "Team: " ++ teamName roster ++ "\n" 
                    ++ battleSpriteStats roster fstSprite ++ "\n" 
                    ++ battleSpriteStats roster sndSprite ++ "\n"
                    ++ battleSpriteStats roster thrSprite

data BoardRoster = BoardRoster [Roster]

instance Show BoardRoster where
    show (BoardRoster rs) = parallelStrings 80 $ map show rs

bSprite :: BattleSprite
bSprite = toBattle (Team 1) (15, 15) $ defaultSprite "def" wyvern

rosterA :: Roster
rosterA = Roster { teamName="A"
                 , team=Team 1
                 , fstSprite=bSprite
                 , sndSprite=bSprite
                 , thrSprite=bSprite
                 }

rosterB :: Roster
rosterB = Roster { teamName="B"
                 , team=Team 2
                 , fstSprite=bSprite
                 , sndSprite=bSprite
                 , thrSprite=bSprite
                 }

bRoster :: BoardRoster
bRoster = BoardRoster [rosterA, rosterB]

data Display = Display { board :: Board, boardRoster :: BoardRoster }

instance Show Display where
    show display = show (boardRoster display) 
                    ++ "------------\n" 
                    ++ show (board display)

display :: Display 
display = Display { board = newBoard 30 30, boardRoster = bRoster }
