module Graphics.Shape (Point, Size, Shape(..), cell) where

import Graphics.CoordinateSystem (CoordinateSystem(..))

type Point = { x :: Number, y :: Number }
type Size = { width :: Number, height :: Number }

data Shape = Rectangle (CoordinateSystem Point) (CoordinateSystem Size)

cell :: CoordinateSystem Point -> Shape
cell origin = Rectangle origin (RelativeToGrid { width: 1.0, height: 1.0 })
