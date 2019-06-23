module Test.Lib.FibrePlane where

import Data.BigInt (toNumber)
import Data.Tuple (fst, snd)
import Effect (Effect)
import Lib.CoordinatePlane (scaleToNewPlane)
import Lib.FibrePlane (ArbitaryCoordinate(..), fibrePlane, preimageInFibrePlane)
import Lib.Util (trustedBigInt)
import Prelude (Unit, bind, pure, show, ($), (<$>), (<<<))
import Test.QuickCheck (quickCheck, (===))
import Test.QuickCheck.Gen as Gen


fibrePlanePreimageTrip :: Effect Unit
fibrePlanePreimageTrip = quickCheck $ \(ArbCoordinate coordinate) -> do
  let coordinateRange = preimageInFibrePlane coordinate
  xInPreimage <- (trustedBigInt <<< show) <$> Gen.choose (toNumber <<< fst $ coordinateRange.xBetween) (toNumber <<< snd $ coordinateRange.xBetween)
  yInPreimage <- (trustedBigInt <<< show) <$> Gen.choose (toNumber <<< fst $ coordinateRange.yBetween) (toNumber <<< snd $ coordinateRange.yBetween)
  let coordinateInPreimage = { x: xInPreimage, y: yInPreimage, plane: fibrePlane }

  let projectedCoordinate = scaleToNewPlane coordinate.plane coordinateInPreimage

  pure $ coordinate === projectedCoordinate

-- hash256Size :: BigInt
-- hash256Size = (fromInt 2) `pow` (fromInt 256)
--
-- fibrePlane :: CoordinatePlane
-- fibrePlane = { xCount: hash256Size, yCount: hash256Size }
--
-- -- Coordinates in the fibrePlane within the resulting coordinate range will all
-- -- map to given coodrinate via scaleToNewPlane
-- preimageInFibrePlane :: Coordinate -> CoordinateRange
-- preimageInFibrePlane { x, y, plane } =
--   { xBetween : Tuple (inverseX x) (inverseX topX)
--   , yBetween : Tuple (inverseY y) (inverseY topY)
--   , plane: fibrePlane
--   }
--   where
--     inverseX x' = scaleToNewSegment plane.xCount fibrePlane.xCount x'
--     inverseY y' = scaleToNewSegment plane.yCount fibrePlane.yCount y'
--     one = trustedBigInt "1"
--     topX = min (x+one) (plane.xCount)
--     topY = min (y+one) (plane.yCount)
