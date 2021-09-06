module Graphics.Drawing (Drawing, DrawingM, fill, renderDrawing) where

import Prelude

import Color (Color)
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.ST (ST, for)
import Data.Array (replicate)
import Data.Array.ST (STArray, new, poke, pushAll, run)
import Graphics.Grid (Cell(..), Grid(..))
import Graphics.CoordinateSystem (relativeToGrid)
import Graphics.Shape (Shape(..))

data DrawingF a
  = Filled Color Shape a

derive instance functorDrawingF :: Functor m => Functor DrawingF

newtype DrawingM a = DrawingM (Free DrawingF a)

derive newtype instance functorDrawingM :: Functor DrawingM
derive newtype instance applyDrawingM :: Apply DrawingM
derive newtype instance applicativeDrawingM :: Applicative DrawingM
derive newtype instance bindDrawingM :: Bind DrawingM
derive newtype instance monadDrawingM :: Monad DrawingM
derive newtype instance semigroupDrawingM :: Semigroup a => Semigroup (DrawingM a)
derive newtype instance monoidDrawingM :: Monoid a => Monoid (DrawingM a)

instance monadRecDrawingM :: MonadRec DrawingM where
  tailRecM k a =
    k a >>= case _ of
      Loop x -> tailRecM k x
      Done y -> pure y

type Drawing = DrawingM Unit

fill :: Color -> Shape -> Drawing
fill color shape = DrawingM $ liftF $ Filled color shape unit

renderDrawing :: Number -> { width :: Int, height :: Int } -> Drawing -> Grid Cell
renderDrawing cellSize g (DrawingM drawing) = Grid array g
  where
  array = run do
    grid <- new
    _ <- flip pushAll grid $ replicate (g.width * g.height) EmptyCell
    foldFree (go grid) drawing
    pure grid

  go :: forall a r. STArray r Cell -> (DrawingF a) -> ST r a
  go grid (Filled color shape cc) = do
    case shape of
      Rectangle origin size -> do
        let
          { x, y } = relativeToGrid cellSize origin
          { width, height } = relativeToGrid cellSize size

        for x (x + width) \i ->
          for y (y + height) \j ->
            poke (j * g.width + i) (Colored color) grid
        pure cc