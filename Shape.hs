module Shape
(
   Point(..)
  ,Shape(..)
  ,surface
  ,baseCircle
  ,sur
  ,nudge
) where



data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float deriving (Show)

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r


surface  :: Shape -> Float
surface (Circle _ r ) = pi * r ^ 2

sur = surface (Circle (Point 0 0 ) 24)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y ) r) a b  = Circle (Point (x + a) (y + b )) r
