module Live (module P, module JS, sXpub, sNode, sPlane, testAddress, testAddressPoint) where

import Lib.AddressTransform as P
import Lib.CoordinatePlane as P
import Lib.FibrePlane (fibrePlane)
import Lib.FibrePlane as P
import Lib.Util as P
import LibJS.BigNumber as JS
import LibJS.Bip32 as JS
import LibJS.Bitcoin (getAddressAtPath)
import LibJS.Bitcoin as JS
import LibJS.HexString as JS

sXpub = JS.XPub "xpub6CUGRUonZSQ4TWtTMmzXdrXDtypWKiKrhko4egpiMZbpiaQL2jkwSB1icqYh2cfDfVxdx4df189oLKnC5fSwqPfgyP3hooxujYzAu3fDVmz"
sNode = JS.mkBip32 sXpub
sPlane = { xCount : P.trustedBigInt "5000", yCount : P.trustedBigInt "3000" }

testAddress i j = JS.getAddressAtPath sNode (JS.mkPath i j)
testAddressPoint i j = P.toPlane sPlane (testAddress i j)

  -- toPlane :: CoordinatePlane -> Address -> Coordinate
