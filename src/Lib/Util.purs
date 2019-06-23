module Lib.Util where

import Data.BigInt as BI
import LibJS.BigNumber as BN
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import Data.Tuple (Tuple)
import LibJS.Bitcoin (hash256Hex)
import Partial.Unsafe (unsafePartial)
import Prelude
import Test.QuickCheck.Arbitrary
import LibJS.HexString

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

hexToBigInt :: HexString -> BI.BigInt
hexToBigInt hex = unsafePartial $ fromRight $ map safeBigNumberToBigInt $ BN.parseBigNumber (show hex)
