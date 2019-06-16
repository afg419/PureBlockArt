module Lib.CoordinatePlane where

import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Prelude (div, sub, ($), (*), (<<<))

import Data.BigInt as BI
import Data.BigNumber as BN
import Data.Tuple (Tuple)
import Partial.Unsafe (unsafePartial)

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

safeBigIntToBigNumber :: BI.BigInt -> BN.BigNumber
safeBigIntToBigNumber bi = unsafePartial (fromRight <<< BN.parseBigNumber <<< BI.toString $ bi)

-- Truncates big number and casts to big int.
safeBigNumberToBigInt :: BN.BigNumber -> BI.BigInt
safeBigNumberToBigInt bn = convert $ BN.decimalPlaces (bn `sub` point5) 0
  where
    point5 = unsafePartial $ fromRight $ BN.parseBigNumber "0.5"
    convert = unsafePartial $ fromJust <<< BI.fromString <<< BN.toString

trustedBigNumber :: String -> BN.BigNumber
trustedBigNumber x = unsafePartial $ fromRight <<< BN.parseBigNumber $ x

trustedBigInt :: String -> BI.BigInt
trustedBigInt x = unsafePartial $ fromJust <<< BI.fromString $ x
