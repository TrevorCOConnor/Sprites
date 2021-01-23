import Board

data Shape = Circle | Square | Triangle

type Origin = Position
type Radius = Int
type Size   = Int

createCircle :: Origin -> Radius -> [Position] 
createCircle (x, y) 0 = [(x, y)]
createCircle (x, y) r = [(c t, s t) | t <- [0.0, step .. 2.0] ] ++ createCircle (x, y) (r-1)
        where r' = fromIntegral r :: Float
              c t = x + (round $ r' * ( cos $ t * pi))
              s t = y + (round $ r' * ( sin $ t * pi))
              step = 0.01

createSquare :: Origin -> Size -> [Position]
createSquare (x, y) 0 = [(x,y)]
createSquare (x, y) s = [(x + tx, y + ty) | tx <- seq, ty <- seq]
    where seq = [(-s), (1-s) .. s]
