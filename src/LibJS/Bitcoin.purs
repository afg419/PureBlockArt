module LibJS.Bitcoin where

import Prelude (show, ($), (<<<), (>>>))
import Data.Maybe (fromJust)
import Partial.Unsafe (unsafePartial)
import LibJS.HexString (HexString, mkHexString)
import LibJS.Bip32 (Address(..), DerivationPath, XPub(..))

hash256Hex :: String -> HexString
hash256Hex hex = unsafePartial $ fromJust <<< mkHexString $ ffiHash256Hex hex
foreign import ffiHash256Hex :: String -> String

foreign import data Bip32Node :: Type

mkBip32 :: XPub -> Bip32Node
mkBip32 (XPub xpub) = ffiMkBip32 xpub
foreign import ffiMkBip32 :: String -> Bip32Node

derivePath :: Bip32Node -> DerivationPath -> Bip32Node
derivePath n = show >>> ffiDerivePath n
foreign import ffiDerivePath :: Bip32Node -> String -> Bip32Node

getAddress :: Bip32Node -> Address
getAddress = ffiGetAddress >>> Address
foreign import ffiGetAddress :: Bip32Node -> String

getAddressAtPath :: Bip32Node -> DerivationPath -> Address
getAddressAtPath node = derivePath node >>> getAddress
