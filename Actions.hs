module Actions where


newtype Power = Power Int
newtype DamageType = DamageType Int
newtype ActionRange = ActionRange Int
newtype Cost = Cost Int
newtype TargetCount = TargetCount Int


data ActionType = Attack | ActionMovement


data TargetType = Self | Selection


data Action = Action { actionType :: ActionType
                     , actionPower :: Power
                     , actionDamageType :: DamageType 
                     , actionRange :: ActionRange
                     , actionCost :: Cost
                     , actionTargetType :: TargetType
                     , actionTartgetCount :: Maybe TargetCount
                     }
