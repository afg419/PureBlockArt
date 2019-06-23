module LibJS.Bip32 where

import Prelude (class Show, show, (<>))

import Data.List (List(..), (:))

data XPub = XPub String
data Address = Address String
instance showAddress :: Show Address where
  show (Address a) = a

data DerivationPath = DerivationPath (List Int)
mkPath :: Int -> Int -> DerivationPath
mkPath i j = DerivationPath (i:j:Nil)
instance showPath :: Show DerivationPath where
  show (DerivationPath Nil) = ""
  show (DerivationPath (a:Nil)) = show a
  show (DerivationPath (a:as)) = show a <> "/" <> show (DerivationPath as)
