module Vision where

import Field
import Movement

distance :: Position -> Position -> Float
distance (a, b) (x, y) = sqrt((x'-a')**2 + (y'-b')**2)
    where a' = fromIntegral a :: Float
          b' = fromIntegral b :: Float
          x' = fromIntegral x :: Float
          y' = fromIntegral y :: Float

radialVision :: Int -> Position -> Field -> Field
radialVision radius position = mapField withinRadius
    where radius' = fromIntegral radius :: Float
          withinRadius sqr = if distance (sqrPosition sqr) position < radius' 
                             then makeVisible sqr 
                             else makeObscure sqr

linearVision' :: Int -> Position -> Field -> Field
linearVision' radius pos field = mapField withinLine field
    where withinLine sqr = if inLineOfSight' radius pos (sqrPosition sqr) field
                              then makeVisible sqr
                              else makeObscure sqr

lineOfSight' :: Field -> Position -> Position -> [Position]
lineOfSight' field p1 p2 = if p1 == p2 || nextstep == p2
                               then []
                               else nextstep : lineOfSight' field nextstep p2
    where nextstep = nextStep' field p1 p2

nextStep :: Position -> Position -> Position
nextStep p1 p2 = if distance xstep p2 < distance ystep p2
                    then xstep
                    else ystep
    where xstep = xStep p1 p2
          ystep = yStep p1 p2

losForgiveness :: Float
losForgiveness = 0.5

nextStep' :: Field -> Position -> Position -> Position
nextStep' field p1 p2
    | xdist - ydist > losForgiveness = ystep
    | ydist - xdist > losForgiveness = xstep
    | otherwise = if isVacant $ getSquare xstep field
                     then xstep
                     else ystep
    where xstep = xStep p1 p2
          ystep = yStep p1 p2
          xdist = distance xstep p2
          ydist = distance ystep p2

yStep :: Position -> Position -> Position
yStep (a, b) (x, y) = if b < y
                         then (a, b+1)
                         else (a, b-1)

xStep :: Position -> Position -> Position
xStep (a, b) (x, y) = if a < x
                         then (a+1, b)
                         else (a-1, b)

inLineOfSight' :: Int -> Position -> Position -> Field -> Bool
inLineOfSight' radius p1 p2 field = if sqrVisibility sqr && distance p1 p2 < radius'
                                      then all (isVacant . getSqr) $ lineOfSight' field p1 p2
                                      else False
    where sqr = getSquare p2 field 
          getSqr p = getSquare p field
          radius' = fromIntegral radius :: Float
