module Lib.CoordinatePlane where

import Data.BigInt as BI
import Data.Tuple (Tuple)
import Lib.Util (hexToBigInt, safeBigIntToBigNumber, safeBigNumberToBigInt)
import LibJS.Bitcoin (hash256Hex)
import Prelude (bind, div, max, min, pure, ($), (*), (<$>), (<<<))


type CoordinatePlane = { xCount :: BI.BigInt, yCount :: BI.BigInt }
type Coordinate = { x :: BI.BigInt, y :: BI.BigInt, plane :: CoordinatePlane }
type CoordinateRange = { xBetween :: Tuple BI.BigInt BI.BigInt , yBetween :: Tuple BI.BigInt BI.BigInt, plane :: CoordinatePlane }

scaleToNewPlane :: Coordinate -> CoordinatePlane -> Coordinate
scaleToNewPlane coordinate newPlane =
  { x: scaleToNewSegment coordinate.plane.xCount newPlane.xCount coordinate.x
  , y: scaleToNewSegment coordinate.plane.yCount newPlane.yCount coordinate.y
  , plane: newPlane
  }

scaleToNewSegment :: BI.BigInt -> BI.BigInt -> BI.BigInt -> BI.BigInt
scaleToNewSegment initialCount finalCount initialPoint =
  safeBigNumberToBigInt (bnInitialPoint * bnFinalCount `div` bnInitialCount)
  where
    bnInitialCount = safeBigIntToBigNumber initialCount
    bnFinalCount = safeBigIntToBigNumber finalCount
    bnInitialPoint = safeBigIntToBigNumber initialPoint
