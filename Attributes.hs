module Attributes where


-- Definitions
newtype HealthPoints = HealthPoints Int
newtype Speed = Speed Int
newtype Stamina = Stamina Int
newtype PhysicalAttack = PhysicalAttack Int
newtype MagicalAttack = MagicalAttack Int
newtype PhysicalDefense = PhysicalDefense Int
newtype MagicalDefense = MagicalDefense Int


class AttributeValue a where
    updateAttribute :: a -> Attributes -> Attributes

    extractAttributeInt :: a -> Int

    embedAttribute :: Int -> a

    extractAttributeFloat :: a -> Float
    extractAttributeFloat x = fromIntegral (extractAttributeInt x) :: Float

    applyToAttributes :: (Int -> Int -> Int) -> a -> a -> a
    applyToAttributes func x y = embedAttribute $ func x' y'
        where x' = extractAttributeInt x
              y' = extractAttributeInt y


instance AttributeValue HealthPoints where
    updateAttribute hp attrs = attrs {healthPoints=hp}
    extractAttributeInt (HealthPoints hp) = hp
    embedAttribute x = HealthPoints x


instance AttributeValue Speed where
    updateAttribute spd attrs = attrs {speed=spd}
    extractAttributeInt (Speed spd) = spd
    embedAttribute x = Speed x
    

instance AttributeValue Stamina where
    updateAttribute stm attrs = attrs {stamina=stm}
    extractAttributeInt (Stamina stm) = stm
    embedAttribute x = Stamina x


instance AttributeValue PhysicalAttack where
    updateAttribute phyAtk attrs = attrs {physicalAttack=phyAtk}
    extractAttributeInt (PhysicalAttack phyAtk) = phyAtk
    embedAttribute x = PhysicalAttack x


instance AttributeValue MagicalAttack where
    updateAttribute magAtk attrs = attrs {magicalAttack=magAtk}
    extractAttributeInt (MagicalAttack magAtk) = magAtk
    embedAttribute x = MagicalAttack x


instance AttributeValue PhysicalDefense where
    updateAttribute phyDef attrs = attrs {physicalDefense=phyDef}
    extractAttributeInt (PhysicalDefense phyDef) = phyDef
    embedAttribute x = PhysicalDefense x


instance AttributeValue MagicalDefense where
    updateAttribute magDef attrs = attrs {magicalDefense=magDef}
    extractAttributeInt (MagicalDefense magDef) = magDef
    embedAttribute x = MagicalDefense x


data Attributes = Attributes { healthPoints :: HealthPoints
                             , speed :: Speed
                             , stamina :: Stamina
                             , physicalAttack :: PhysicalAttack
                             , magicalAttack :: MagicalAttack
                             , physicalDefense :: PhysicalDefense
                             , magicalDefense :: MagicalDefense
                             }
