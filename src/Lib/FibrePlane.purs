module Lib.FibrePlane where

import Prelude

import Data.BigInt (BigInt, fromInt, pow)
import Data.Tuple (Tuple(..))
import Lib.CoordinatePlane (Coordinate, trustedBigInt, CoordinatePlane, CoordinateRange, scaleToNewSegment)
import Partial.Unsafe (unsafePartial)
import Data.Either (fromRight)


hash256Size :: BigInt
hash256Size = (fromInt 2) `pow` (fromInt 256)

fibrePlane :: CoordinatePlane
fibrePlane = { xCount: hash256Size, yCount: hash256Size }

-- Coordinates in the fibrePlane within the resulting coordinate range will all
-- map to given coodrinate via scaleToNewPlane
preimageInFibrePlane :: Coordinate -> CoordinateRange
preimageInFibrePlane { x, y, plane } =
  { xBetween : Tuple (inverseX x) (inverseX topX)
  , yBetween : Tuple (inverseY y) (inverseY topY)
  , plane: fibrePlane
  }
  where
    inverseX x' = scaleToNewSegment plane.xCount fibrePlane.xCount x'
    inverseY y' = scaleToNewSegment plane.yCount fibrePlane.yCount y'
    one = trustedBigInt "1"
    topX = min (x+one) (plane.xCount)
    topY = min (y+one) (plane.yCount)
