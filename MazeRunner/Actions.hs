module Actions where

import Field
import Movement

data ActionMap = ActionMap { key :: Char
                           , action :: IO Action }

data Action = Movement (Position -> Field -> (Position, Field)) 
                | ModifyField (Position -> Field -> Field)
                | Escape
