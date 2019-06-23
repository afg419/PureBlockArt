module Lib.CoordinatePlane where

import Data.BigInt as BI
import LibJS.BigNumber as BN
import Data.DivisionRing
import Data.Tuple (Tuple)
import Lib.Util (safeBigIntToBigNumber, safeBigNumberToBigInt)
import Prelude (div, (*))

type CoordinatePlane = { xCount :: BI.BigInt, yCount :: BI.BigInt }
type Coordinate = { x :: BI.BigInt, y :: BI.BigInt, plane :: CoordinatePlane }
type CoordinateRange = { xBetween :: Tuple BI.BigInt BI.BigInt , yBetween :: Tuple BI.BigInt BI.BigInt, plane :: CoordinatePlane }

scaleToNewPlane :: CoordinatePlane -> Coordinate -> Coordinate
scaleToNewPlane newPlane coordinate  =
  { x: scaleToNewSegment coordinate.plane.xCount newPlane.xCount coordinate.x
  , y: scaleToNewSegment coordinate.plane.yCount newPlane.yCount coordinate.y
  , plane: newPlane
  }

scaleToNewSegment :: BI.BigInt -> BI.BigInt -> BI.BigInt -> BI.BigInt
scaleToNewSegment initialCount finalCount initialPoint =
  safeBigNumberToBigInt ((bnInitialPoint * bnFinalCount) `rightDiv` bnInitialCount)
  where
    bnInitialCount = BN.precision (safeBigIntToBigNumber initialCount) 256
    bnFinalCount = BN.precision (safeBigIntToBigNumber finalCount) 256
    bnInitialPoint = BN.precision (safeBigIntToBigNumber initialPoint) 256
