module Game.Grid where

import Prelude
import Color (Color)
import Data.Array (replicate, (!!), modifyAtIndices)
import Data.Maybe (Maybe)

data Cell
  = Empty
  | Filled Color

type Grid
  = Array (Array Cell)

empty :: Int -> Int -> Grid
empty width height = replicate width (replicate height Empty)

index :: Grid -> Int -> Int -> Maybe Cell
index grid x y = do
  col <- grid !! x
  cell <- col !! y
  pure cell

set :: Grid -> Int -> Int -> Cell -> Grid
set grid x y cell = modifyAtIndices [ x ] (\col -> modifyAtIndices [ y ] (const cell) col) grid
