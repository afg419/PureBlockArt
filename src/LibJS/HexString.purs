module LibJS.HexString where

import Data.BigInt as BI
import Data.BigNumber as BN
import Data.BigNumber as BN.BigNumber
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..), fromCodePointArray, indexOf, takeWhile)
import Prelude (class Show, show, ($), (==), (<>), map)

hexAlph :: String
hexAlph = "0123456789abcdefABCDEF"

newtype HexString = HexString String
instance showHexString :: Show HexString where
  show (HexString hs) = "0x" <> hs

mkHexString :: String -> Maybe HexString
mkHexString s = if takeWhile inHexAlph s == s
  then Just $ HexString s
  else Nothing
  where
    inHexAlph c = isJust (indexOf (Pattern $ fromCodePointArray [c]) hexAlph)
