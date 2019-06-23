module LibJS.BigNumber where

import Prelude
import Data.Int as Int
import Data.Either (Either (..))
import Data.Function.Uncurried (Fn3, runFn3, Fn2, runFn2, Fn5, runFn5)
import Data.Generic.Rep (class Generic, Constructor (..), Argument (..))
import Effect (Effect)
import Effect.Exception (Error)
import Partial.Unsafe (unsafePartial)

foreign import data BigNumber :: Type

instance genericBigNumber :: Generic BigNumber (Constructor "BigNumber" (Argument String)) where
  from x = Constructor (Argument (toString x))
  to (Constructor (Argument s)) = unsafePartial $ case parseBigNumber s of
    Right x -> x

foreign import parseBigNumberImpl :: Fn3 (forall e a. e -> Either e a) (forall e a. a -> Either e a) String (Either Error BigNumber)

parseBigNumber :: String -> Either Error BigNumber
parseBigNumber = runFn3 parseBigNumberImpl Left Right

foreign import isBigNumber :: forall a. a -> Boolean
foreign import isInteger :: BigNumber -> Boolean
foreign import isFinite :: BigNumber -> Boolean
foreign import isNaN :: BigNumber -> Boolean
foreign import isNegative :: BigNumber -> Boolean
foreign import isPositive :: BigNumber -> Boolean
foreign import isZero :: BigNumber -> Boolean

foreign import toNumber :: BigNumber -> Number
foreign import toString :: BigNumber -> String
foreign import toExponential :: BigNumber -> String
foreign import toFixed :: BigNumber -> String
foreign import toFormat :: BigNumber -> String
foreign import valueOf :: BigNumber -> String

foreign import eqBigNumberImpl :: Fn2 BigNumber BigNumber Boolean
foreign import gtBigNumberImpl :: Fn2 BigNumber BigNumber Boolean
foreign import gteBigNumberImpl :: Fn2 BigNumber BigNumber Boolean
foreign import ltBigNumberImpl :: Fn2 BigNumber BigNumber Boolean
foreign import lteBigNumberImpl :: Fn2 BigNumber BigNumber Boolean
foreign import compareBigNumberImpl :: Fn5 Ordering Ordering Ordering BigNumber BigNumber Ordering

foreign import absImpl :: BigNumber -> BigNumber
foreign import plusBigNumberImpl :: Fn2 BigNumber BigNumber BigNumber
foreign import minusBigNumberImpl :: Fn2 BigNumber BigNumber BigNumber
foreign import negateImpl :: BigNumber -> BigNumber
foreign import timesBigNumberImpl :: Fn2 BigNumber BigNumber BigNumber
foreign import divBigNumberImpl :: Fn2 BigNumber BigNumber BigNumber
foreign import idivBigNumberImpl :: Fn2 BigNumber BigNumber BigNumber
foreign import moduloBigNumberImpl :: Fn2 BigNumber BigNumber BigNumber
foreign import sqrt :: BigNumber -> BigNumber
foreign import powBigNumberImpl :: Fn2 BigNumber BigNumber BigNumber

abs' :: BigNumber -> BigNumber
abs' = absImpl

negate' :: BigNumber -> BigNumber
negate' = negateImpl

idiv :: BigNumber -> BigNumber -> BigNumber
idiv = runFn2 idivBigNumberImpl

pow :: BigNumber -> BigNumber -> BigNumber
pow = runFn2 powBigNumberImpl

foreign import intValue :: BigNumber -> BigNumber
foreign import precisionImpl :: Fn2 BigNumber Int BigNumber
foreign import decimalPlacesImpl :: Fn2 BigNumber Int BigNumber
foreign import randomBigNumber :: Effect BigNumber
foreign import shiftedByImpl :: Fn2 BigNumber Int BigNumber

precision :: BigNumber -> Int -> BigNumber
precision = runFn2 precisionImpl

decimalPlaces :: BigNumber -> Int -> BigNumber
decimalPlaces = runFn2 decimalPlacesImpl

shiftedBy :: BigNumber -> Int -> BigNumber
shiftedBy = runFn2 shiftedByImpl




instance eqBigNumber :: Eq BigNumber where
  eq = runFn2 eqBigNumberImpl

instance ordBigNumber :: Ord BigNumber where
  compare = runFn5 compareBigNumberImpl LT EQ GT

instance showBigNumber :: Show BigNumber where
  show = toString

instance semiringBigNumber :: Semiring BigNumber where
  add = runFn2 plusBigNumberImpl
  zero = unsafePartial $ case parseBigNumber "0" of
    Right x -> x
  one = unsafePartial $ case parseBigNumber "1" of
    Right x -> x
  mul = runFn2 timesBigNumberImpl

instance ringBigNumber :: Ring BigNumber where
  sub = runFn2 minusBigNumberImpl

instance commutativeRingBigNumber :: CommutativeRing BigNumber

instance divisionRingBigNumber :: DivisionRing BigNumber where
  recip = runFn2 divBigNumberImpl one

instance euclideanRingBigNumber :: EuclideanRing BigNumber where
  degree = Int.floor <<< toNumber <<< intValue <<< abs'
  div = runFn2 divBigNumberImpl
  mod = runFn2 moduloBigNumberImpl
