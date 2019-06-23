module Lib.FibrePlane where

import Prelude

import Data.BigInt (BigInt, fromInt, pow, rem)
import Data.Tuple (Tuple(..))
import Lib.CoordinatePlane (Coordinate, CoordinatePlane, CoordinateRange, scaleToNewPlane, scaleToNewSegment)
import Lib.Util (hexToBigInt, trustedBigInt)
import LibJS.Bitcoin (hash256Hex)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

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

newtype ArbitaryCoordinate = ArbCoordinate Coordinate
instance arbCoordinate :: Arbitrary ArbitaryCoordinate where
  arbitrary = do
    x1 <- (hexToBigInt <<< hash256Hex) <$> arbitrary
    y1 <- (hexToBigInt <<< hash256Hex) <$> arbitrary
    x2 <- (flip rem (trustedBigInt "100000000") <<< hexToBigInt <<< hash256Hex) <$> arbitrary
    y2 <- (flip rem (trustedBigInt "100000000") <<< hexToBigInt <<< hash256Hex) <$> arbitrary

    let coordinate = { x: x1, y: y1, plane : fibrePlane }
    let plane = { xCount : x2, yCount : y2 }
    pure <<< ArbCoordinate $ scaleToNewPlane plane coordinate
