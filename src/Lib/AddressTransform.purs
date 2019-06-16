module Lib.AddressTransform where

import Prelude
import Data.BigInt (BigInt, fromInt)

import Lib.CoordinatePlane (Coordinate)
import Lib.FibrePlane (fibrePlane)

newtype Address = Address String

-- TODO: figure out how to concatenate strings, jesus
toFibrePlane :: Address -> Coordinate
toFibrePlane (Address addr) = { x: hash256BigInt xShift, y: hash256BigInt yShift, plane: fibrePlane }
  where
    xShift = "x" <> addr
    yShift = "y" <> addr

-- TODO: write this as an FFI from js lib
hash256BigInt :: String -> BigInt
hash256BigInt digest = fromInt 0
