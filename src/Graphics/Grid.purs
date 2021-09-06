module Graphics.Grid (Grid(..), Cell(..), enumerate, differencesFrom) where

import Prelude

import Color (Color)
import Data.Array (filter, mapWithIndex, zipWith)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))

data Grid a = Grid (Array a) { width :: Int, height :: Int }

data Cell = Colored Color | EmptyCell
derive instance eqCell :: Eq Cell

enumerate :: forall a. Grid a -> Array (Tuple { x :: Int, y :: Int } a)
enumerate (Grid xs { width }) = enumerate2D width xs

enumerate2D :: forall a. Int -> Array a -> Array (Tuple { x :: Int, y :: Int } a)
enumerate2D width = mapWithIndex go
  where
  go i = Tuple { x: i `mod` width, y: i / width }

differencesFrom ::
  forall a.
  Eq a =>
  Grid a ->
  Grid a ->
  Array (Tuple { x :: Int, y :: Int } a)
differencesFrom (Grid cells cfg@{ width }) (Grid cellsReference cfgReference)
  | cfg /= cfgReference = enumerate2D width cells
  | otherwise = map (\(Tuple i (x /\ _)) -> Tuple i x)
      $ filter (\((Tuple _ (x /\ y))) -> x /= y)
      $ enumerate2D width
      $ (zipWith Tuple cells cellsReference)
