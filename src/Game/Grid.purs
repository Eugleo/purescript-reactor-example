module Game.Grid where

import Prelude

import Color (Color)
import Control.Monad.Free (Free, foldFree, liftF)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRecM)
import Control.Monad.ST (ST, for)
import Data.Array (mapWithIndex, replicate)
import Data.Array.ST (STArray, new, poke, pushAll, run)
import Data.Int (floor, toNumber)
import Data.Tuple (Tuple(..))
import Graphics.CanvasAction (CanvasStyleRep, toStyleRep) as Canvas
import Heterogeneous.Mapping (class HMap, hmap)

type Point = { x :: Number, y :: Number }
type Size = { width :: Number, height :: Number }
type Region = { x :: Int, y :: Int, height :: Int, width :: Int }

data Shape = Rectangle (CoordinateSystem Point) (CoordinateSystem Size)

data CoordinateSystem a = RelativeToGrid a | RelativeToCanvas a
derive instance functorCoordinateSystem :: Functor CoordinateSystem

wrt :: forall a b. a -> (a -> CoordinateSystem b) -> CoordinateSystem b
wrt = (#)

canvas :: forall a. a -> CoordinateSystem a
canvas = RelativeToCanvas

grid :: forall a b. HMap (Int -> Number) a b => a -> CoordinateSystem b
grid = RelativeToGrid <<< hmap (\n -> toNumber n)

relativeToGrid ::
  forall a b.
  HMap (Number -> Int) a b =>
  Number ->
  CoordinateSystem a ->
  b
relativeToGrid _ (RelativeToGrid x) = hmap (\n -> floor n) x
relativeToGrid cellSize (RelativeToCanvas x) = hmap (\n -> floor (n / cellSize)) x

cell :: CoordinateSystem Point -> Shape
cell origin = Rectangle origin (RelativeToGrid { width: 1.0, height: 1.0 })

data DrawingF a
  = Filled Canvas.CanvasStyleRep Shape a

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
fill color shape = DrawingM $ liftF $ Filled (Canvas.toStyleRep color) shape unit

data Cell = Styled Canvas.CanvasStyleRep | EmptyCell

data Grid = Grid (Array Cell) { width :: Int, height :: Int }

enumerate :: Grid -> Array (Tuple { x :: Int, y :: Int } Cell)
enumerate (Grid cells { width }) = mapWithIndex go cells
  where
  go i = Tuple { x: i `mod` width, y: i / width }

differencesFrom :: Grid -> Grid -> Array (Tuple { x :: Int, y :: Int } Cell)
differencesFrom g@(Grid cells cfg) (Grid cellsReference cfgReference)
  | cfg /= cfgReference = enumerate g
  | otherwise = enumerate g

renderDrawing :: Number -> { width :: Int, height :: Int } -> Drawing -> Grid
renderDrawing cellSize g (DrawingM drawing) = Grid array g
  where
  array = run do
    grid <- new
    _ <- flip pushAll grid $ replicate (g.width * g.height) EmptyCell
    foldFree (go grid) drawing
    pure grid

  go :: forall a r. STArray r Cell -> (DrawingF a) -> ST r a
  go grid (Filled style shape cc) = do
    case shape of
      Rectangle origin size -> do
        let
          { x, y } = relativeToGrid cellSize origin
          { width, height } = relativeToGrid cellSize size

        for x (x + width) \i ->
          for y (y + height) \j ->
            poke (j * g.width + i) (Styled style) grid
        pure cc
