module Actions where

import Field
import Movement
import DisplayMethod
import Data.Maybe

data MovementKeys = MovementKeys { upKey :: Char
                                  , downKey :: Char
                                  , leftKey :: Char
                                  , rightKey :: Char
                                  }

data ActionMap = ActionMap { key :: Char
                           , action :: Action }

data Action = Movement (Position -> Field -> (Position, Field)) 
                | ModifyField (Position -> Field -> Field)
                | ModifyView (Field -> Field)
                | ChangeView 
                | ToggleTraversed
                | EmptyAction
                | Escape
                | JointAction Action Action
                | DirectionMap (MovementKeys -> [ActionMap])


toMap :: [ActionMap] -> [(Char, Action)]
toMap = map toTuple


toTuple :: ActionMap -> (Char, Action)
toTuple am = (key am, action am)


joinActions :: Action -> Action -> Action
joinActions a1 a2 =  JointAction a1 a2 


mapDirectionKeys :: (Position -> Field -> Field) -> MovementKeys -> [ActionMap]
mapDirectionKeys func mks = [ ActionMap { key=upKey mks
                                        , action=ModifyField $ func . up}
                            , ActionMap { key=leftKey mks
                                        , action=ModifyField $ func . left}
                            , ActionMap { key=downKey mks
                                        , action=ModifyField $ func . down}
                            , ActionMap { key=rightKey mks
                                        , action=ModifyField $ func . right}
                            ]


createMovementMap :: MovementKeys -> [ActionMap]
createMovementMap mks = [ ActionMap { key=upKey mks
                                    , action=Movement moveUp }
                        , ActionMap { key=leftKey mks
                                    , action=Movement moveLeft }
                        , ActionMap { key=downKey mks
                                    , action=Movement moveDown }
                        , ActionMap { key=rightKey mks
                                    , action=Movement moveRight }
                        ]

-- Actions

moveUpAction :: Action
moveUpAction =  Movement moveUp

moveDownAction :: Action
moveDownAction = Movement moveDown

moveLeftAction :: Action
moveLeftAction = Movement moveLeft

moveRightAction :: Action
moveRightAction = Movement moveRight

placeObjAction :: Action
placeObjAction = DirectionMap $ mapDirectionKeys safePlaceObjOnField

removeObjAction :: Action
removeObjAction = DirectionMap $ mapDirectionKeys makePositionVacant

markAction :: Action
markAction =  DirectionMap $ mapDirectionKeys $ modifySquare (markSquare 'x')

seeMapAction :: Action
seeMapAction = ModifyView (mapField makeVisible)
