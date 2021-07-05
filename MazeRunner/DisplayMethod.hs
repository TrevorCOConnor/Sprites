module DisplayMethod where

import Field
import Movement
import View
import Vision
import Data.Maybe

type DisplayFunction = (Position -> Field -> IO())
type VisionFunction = (Position -> Field -> Field)

data DisplayMethod = DisplayMethod { name :: [Char]
                                   , view :: (Position -> Field -> IO()) 
                                   , vision :: (Position -> Field -> Field)
                                   , showTraversed :: Bool
                                   }

instance Eq DisplayMethod where
    dm1 == dm2 = (name dm1) == (name dm2)


radius :: Int
radius = 30


vRadius :: Float
vRadius = 30


visualMethod :: DisplayMethod
visualMethod = DisplayMethod { name="Visual Method"
                             , view=printVisualMethod 
                             , vision=linearVision radius
                             , showTraversed=False
                             }


linearMethod :: DisplayMethod
linearMethod = DisplayMethod { name="Linear Method"
                             , view=printFullMethod
                             , vision=linearVision radius
                             , showTraversed=False
                             }


easyMethod :: DisplayMethod
easyMethod = DisplayMethod { name="Easy Method"
                           , view=printFullMethod
                           , vision=noVision
                           , showTraversed=False
                           }


printVisualMethod :: Position -> Field -> IO ()
printVisualMethod pos field = print view
    where view = View { viewRadius=vRadius 
                      , viewField=field
                      , viewFocalPoint=pos
                      }


printFullMethod :: Position -> Field -> IO ()
printFullMethod pos = putStr . wholeFieldView


method :: DisplayMethod -> DisplayFunction
method dm = if showTraversed dm
               then traversedFunction 
               else regularFunction
   where traversedFunction pos field = (view dm) pos $ markTraversedVisible
                                                     $ (vision dm) pos field 
         regularFunction pos field = (view dm) pos $ (vision dm) pos field


markTraversedVisible :: Field -> Field
markTraversedVisible field = mapField makeTraversedVisible field 
    where makeTraversedVisible sqr = if sqrTraversed sqr
                                        then makeVisible sqr 
                                        else sqr
