module Lib.AddressTransform where

import Prelude

import Lib.CoordinatePlane (Coordinate, CoordinatePlane, scaleToNewPlane)
import Lib.FibrePlane (fibrePlane)
import Lib.Util (hexToBigInt)
import LibJS.Bip32 (Address(..))
import LibJS.Bitcoin (hash256Hex)

toFibrePlane :: Address -> Coordinate
toFibrePlane (Address addr) = { x: toPoint xShift, y: toPoint yShift, plane: fibrePlane }
  where
    xShift = "x" <> addr
    yShift = "y" <> addr
    toPoint = hexToBigInt <<< hash256Hex

toPlane :: CoordinatePlane -> Address -> Coordinate
toPlane plane = toFibrePlane >>> scaleToNewPlane plane
