{-# OPTIONS -Wall -fno-warn-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Scope.Types
import Scope.Orphans ()

import Control.Applicative
import Data.List (sort)

import Data.Time.Calendar (Day(..))
import Data.Time.Clock

import Test.QuickCheck
import Test.HUnit hiding (Test)

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

----------------------------------------------------------------------
-- * Derive some instances

deriving instance Arbitrary ScreenX
deriving instance Arbitrary ScreenY
deriving instance Arbitrary CanvasX
deriving instance Arbitrary CanvasY
deriving instance Arbitrary DataX
deriving instance Arbitrary DataY

deriving instance ApproxEq ScreenX
deriving instance ApproxEq ScreenY
deriving instance ApproxEq CanvasX
deriving instance ApproxEq CanvasY
deriving instance ApproxEq DataX
deriving instance ApproxEq DataY

instance (ApproxEq t, Ord t, AffineSpace t) => ApproxEq (Range t) where
    approxEq tol r1 r2 = approxEq tol (toBounds r1) (toBounds r2)

deriving instance Arbitrary p => Arbitrary (Point p)
deriving instance ApproxEq p => ApproxEq (Point p)

deriving instance Arbitrary Day
instance Arbitrary UTCTime where
    arbitrary = do
      day <- arbitrary
      diff <- arbitrary
      return $ UTCTime { utctDay = day, utctDayTime = diff }
    shrink (UTCTime day diff) = UTCTime <$> shrink day <*> shrink diff

instance Arbitrary DiffTime where
    arbitrary = picosecondsToDiffTime <$> arbitrary

instance Arbitrary NominalDiffTime where
    arbitrary = diffUTCTime <$> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (D2V a b) where
    arbitrary = D2V <$> arbitrary <*> arbitrary
    shrink (D2V x y) = D2V <$> shrink x <*> shrink y

instance (ApproxEq a, ApproxEq b) => ApproxEq (D2V a b) where
    approxEq tol (D2V x1 y1) (D2V x2 y2) = approxEq tol (x1,y1) (x2,y2)

instance ApproxEq UTCTime where
    approxEq tol t1 t2 = abs (diffUTCTime t1 t2) < realToFrac (tol*2)

instance ApproxEq NominalDiffTime where
    approxEq tol t1 t2 = abs (t2 - t1) <= realToFrac (tol*20)

-- Although Ranges created by @fromBounds@ should always have a positive
-- distance, there's no reason for that to be true in general, so we allow
-- Arbitrary to create Ranges with negative distance.
instance (Arbitrary r) => Arbitrary (Range r) where
    arbitrary = Range <$> arbitrary <*> arbitrary

-- Don't want to drag in more dependencies for just this.
class ApproxEq a where
    approxEq :: Double -> a -> a -> Bool

instance ApproxEq Double where
    approxEq tol d1 d2 = m == 0.0 || d/m < tol where
      m = max (abs d1) (abs d2)
      d = abs (d1 - d2)

instance (ApproxEq a, ApproxEq b) => ApproxEq (a,b) where
    approxEq tol (l1,r1) (l2,r2) = approxEq tol l1 l2 && approxEq tol r1 r2

infix 4 ===
(===) :: (ApproxEq a) => a -> a -> Bool
(===) = approxEq {-pretty equal-}1.0e-10

----------------------------------------------------------------------
-- * Range tests

-- | When creating '(s0,s1)', s1 >= 0
orderedBoundsRange :: (Ord a, AffineSpace a) => Range a -> Bool
orderedBoundsRange rng = rmax >= rmin
    where (rmin,rmax) = toBounds rng

-- | A created range should always have a non-negative distance
orderedRangeBounds :: (Ord (Diff t), Ord t, AffineSpace t) => Bounds t -> Bool
orderedRangeBounds bounds = (max1 .-. min1) >= zeroV
    where (min1,max1) = toBounds $ fromBounds bounds

roundTripRange :: (Ord a, ApproxEq a, AffineSpace a) => a -> a -> Bool
roundTripRange s0 s1 =
  if s1 >= s0
    then (s0,s1) === (toBounds $ fromBounds (s0,s1) )
    else (s1,s0) === (toBounds $ fromBounds (s0,s1) )


----------------------------------------------------------------------
-- restrictRange tests

restrictOuter1D :: (Eq (Basis (Diff t)), Num (Scalar (Diff t)),
                   Ord (Scalar (Diff t)), Ord t, HasBasis (Diff t), AffineSpace t,
                   ApproxEq t) =>
                   t -> t -> t -> t -> Property
restrictOuter1D v1 v2 v3 v4 = True
    ==> rng === restrictRange restriction rng
    where restriction = fromBounds (r0,r1) -- normalized restriction
          rng = fromBounds (s0,s1)
          [r0,s0,s1,r1] = sort [v1,v2,v3,v4]

restrictInner1D :: (Eq (Basis (Diff t)), Num (Scalar (Diff t)),
                   Ord (Scalar (Diff t)), Ord t, HasBasis (Diff t), AffineSpace t,
                   ApproxEq t) =>
                   t -> t -> t -> t -> Property
restrictInner1D v1 v2 v3 v4 = True
    ==> restriction === restrictRange restriction rng
    where restriction = fromBounds (r0,r1) -- normalized restriction
          rng = fromBounds (s0,s1)
          [s0,r0,r1,s1] = sort [v1,v2,v3,v4]

restrictLeft1D :: (Eq (Basis (Diff t)), Num (Scalar (Diff t)), Ord (Scalar (Diff t)),
                  Ord t, HasBasis (Diff t), AffineSpace t, ApproxEq t) =>
                  t -> t -> t -> t -> Property
restrictLeft1D v1 v2 v3 v4 = True
    ==> fromBounds (r0,s1)
        === restrictRange (fromBounds (r0,r1)) (fromBounds (s0,s1))
   where
      [s0,r0,s1,r1] = sort [v1,v2,v3,v4]

restrictRight1D :: (Eq (Basis (Diff t)), Num (Scalar (Diff t)),
                   Ord (Scalar (Diff t)), Ord t, HasBasis (Diff t), AffineSpace t,
                   ApproxEq t) =>
                   t -> t -> t -> t -> Property
restrictRight1D v1 v2 v3 v4 = True
    ==> fromBounds (s0,r1)
        === restrictRange (fromBounds (r0,r1)) (fromBounds (s0,s1))
    where
      [r0,s0,r1,s1] = sort [v1,v2,v3,v4]

restrictNeg :: (Eq (Basis (Diff t)), Num (Scalar (Diff t)), Ord (Scalar (Diff t)),
               Ord t, HasBasis (Diff t), AffineSpace t, ApproxEq t) =>
               Range t -> Range t -> Bool
restrictNeg rng1 rng2 = restrictRange rng1 rng2 === restrictRange rng1' rng2'
    where
        rng1' = fromBounds $ toBounds rng1
        rng2' = fromBounds $ toBounds rng2

restrictMiss1D :: (Eq (Basis (Diff t)), Num (Scalar (Diff t)), Ord (Scalar (Diff t)),
                  Ord t, HasBasis (Diff t), AffineSpace t, ApproxEq t) =>
                  t -> t -> t -> t -> Property
restrictMiss1D v1 v2 v3 v4 = True
    ==> fromBounds (r0,r0)
        === restrictRange (fromBounds (r0,r1)) (fromBounds (s0,s1))
        -- flip the positions to check misses on both sides
        && fromBounds (s0,s0)
        === restrictRange (fromBounds (s0,s1)) (fromBounds (r0,r1))
   where
      [r0,r1,s0,s1] = sort [v1,v2,v3,v4]


-- 2D tests

restrictD2V :: (Eq (Basis (Diff t1)), Eq (Basis (Diff t)), Num (Scalar (Diff t)),
               Ord (Scalar (Diff t)), Ord t, Ord t1, HasBasis (Diff t1),
               HasBasis (Diff t), AffineSpace t, AffineSpace t1, ApproxEq t1,
               ApproxEq t, Scalar (Diff t1) ~ Scalar (Diff t)) =>
               Range t1 -> Range t1 -> Range t -> Range t -> Bool
restrictD2V xr1 xr2 yr1 yr2 = 
    rngMin === (D2V minx miny) && rngMax === D2V maxx maxy
    where
        Range rngMin rngMax = restrictRange r2d1 r2d2
        (minx,maxx) = toBounds $ restrictRange (xr1) (xr2)
        (miny,maxy) = toBounds $ restrictRange (yr1) (yr2)
        r2d1 = range2D xr1 yr1
        r2d2 = range2D xr2 yr2

-- union tests

unionRangeBounds :: (Num (Scalar (Diff t)), Ord (Scalar (Diff t)), Ord t,
                    HasBasis (Diff t), AffineSpace t, ApproxEq t)
                 => Range t -> Range t -> Bool
unionRangeBounds r1 r2 =
    toBounds (unionRange r1 r2) === (min min1 min2,max max1 max2)
    where
        (min1,max1) = toBounds r1
        (min2,max2) = toBounds r2

viewRangeIdentity r1 = viewRange (fromBoundsC 0 1) r1 === r1

viewRangeNull :: (AffineSpace t, Scalar (Diff t) ~ Double, VectorSpace (Diff t),
                  ApproxEq (Diff t), Num (Diff t))
              => ScreenX -> Range t -> Bool
viewRangeNull x r1 = snd (toSpan (viewRange (fromBoundsC x x) r1)) === 0

----------------------------------------------------------------------
-- Test harness

main :: IO ()
main = defaultMain tests

-- do a *whole lot* of tests, to make sure that the instances are all correct.
tests :: [Test]
tests =
    [ testGroup "bounds"
      [ testGroup "construction"
        [ testProperty "Double" (orderedBoundsRange  :: Range Double  -> Bool)
        , testProperty "ScreenX" (orderedBoundsRange :: Range ScreenX -> Bool)
        , testProperty "ScreenY" (orderedBoundsRange :: Range ScreenY -> Bool)
        , testProperty "CanvasX" (orderedBoundsRange :: Range CanvasX -> Bool)
        , testProperty "CanvasY" (orderedBoundsRange :: Range CanvasY -> Bool)
        , testProperty "DataX" (orderedBoundsRange   :: Range DataX   -> Bool)
        , testProperty "DataY" (orderedBoundsRange   :: Range DataY   -> Bool)
        , testProperty "ScreenP" (orderedBoundsRange :: Range ScreenP -> Bool)
        , testProperty "CanvasP" (orderedBoundsRange :: Range CanvasP -> Bool)
        , testProperty "DataP" (orderedBoundsRange   :: Range DataP   -> Bool)
        ]
      ]
    , testGroup "range"
      [ testGroup "construction"
        [ testProperty "Double" (orderedRangeBounds  :: (Double,Double)->Bool)
        , testProperty "ScreenX" (orderedRangeBounds :: (ScreenX,ScreenX)->Bool)
        , testProperty "ScreenY" (orderedRangeBounds :: (ScreenY,ScreenY)->Bool)
        , testProperty "CanvasX" (orderedRangeBounds :: (CanvasX,CanvasX)->Bool)
        , testProperty "CanvasY" (orderedRangeBounds :: (CanvasY,CanvasY)->Bool)
        , testProperty "DataX" (orderedRangeBounds   :: (DataX  ,DataX)->Bool)
        , testProperty "DataY" (orderedRangeBounds   :: (DataY  ,DataY)->Bool)
        , testProperty "ScreenP" (orderedRangeBounds :: (ScreenP,ScreenP)->Bool)
        , testProperty "CanvasP" (orderedRangeBounds :: (CanvasP,CanvasP)->Bool)
        , testProperty "DataP" (orderedRangeBounds   :: (DataP  ,DataP)->Bool)
        ]
      ]
    , testGroup "roundTrip"
      [ testProperty "Double" (roundTripRange :: Double->Double->Bool)
      , testProperty "ScreenX" (roundTripRange :: ScreenX->ScreenX->Bool)
      , testProperty "ScreenY" (roundTripRange :: ScreenY->ScreenY->Bool)
      , testProperty "CanvasX" (roundTripRange :: CanvasX->CanvasX->Bool)
      , testProperty "CanvasY" (roundTripRange :: CanvasY->CanvasY->Bool)
      , testProperty "DataX" (roundTripRange :: DataX->DataX->Bool)
      , testProperty "DataY" (roundTripRange :: DataY->DataY->Bool)
      , testProperty "ScreenP" (roundTripRange :: ScreenP->ScreenP->Bool)
      , testProperty "CanvasP" (roundTripRange :: CanvasP->CanvasP->Bool)
      , testProperty "DataP" (roundTripRange :: DataP->DataP->Bool)
      ]
    , testGroup "union"
    -- only testing with a few types, if there are problems with instances
    -- they'll show up in restrictRange tests
    [ testProperty "Double"
        (unionRangeBounds :: Range Double->Range Double->Bool)
    , testProperty "ScreenX"
        (unionRangeBounds :: Range ScreenX->Range ScreenX->Bool)
    , testProperty "NominalDiffTime"
        (unionRangeBounds :: Range NominalDiffTime->Range NominalDiffTime->Bool)
    ]
    , testGroup "restrictRange"
      [ testGroup "restrictOuter1D"
        [ testProperty "Double"
            (restrictOuter1D  :: Double->Double->Double->Double->Property)
        , testProperty "ScreenX"
            (restrictOuter1D :: ScreenX->ScreenX->ScreenX->ScreenX->Property)
        , testProperty "ScreenY"
            (restrictOuter1D :: ScreenY->ScreenY->ScreenY->ScreenY->Property)
        , testProperty "CanvasX"
            (restrictOuter1D :: CanvasX->CanvasX->CanvasX->CanvasX->Property)
        , testProperty "CanvasY"
            (restrictOuter1D :: CanvasY->CanvasY->CanvasY->CanvasY->Property)
        , testProperty "DataX"
            (restrictOuter1D   :: DataX-> DataX->DataX-> DataX->Property)
        , testProperty "DataY"
            (restrictOuter1D   :: DataY-> DataY->DataY-> DataY->Property)
        , testProperty "NominalDiffTime"
            (restrictOuter1D :: NominalDiffTime->NominalDiffTime
                                ->NominalDiffTime->NominalDiffTime->Property)
        ]
     , testGroup "restrictInner1D"
        [ testProperty "Double"
            (restrictInner1D  :: Double->Double->Double->Double->Property)
        , testProperty "ScreenX"
            (restrictInner1D :: ScreenX->ScreenX->ScreenX->ScreenX->Property)
        , testProperty "ScreenY"
            (restrictInner1D :: ScreenY->ScreenY->ScreenY->ScreenY->Property)
        , testProperty "CanvasX"
            (restrictInner1D :: CanvasX->CanvasX->CanvasX->CanvasX->Property)
        , testProperty "CanvasY"
            (restrictInner1D :: CanvasY->CanvasY->CanvasY->CanvasY->Property)
        , testProperty "DataX"
            (restrictInner1D   :: DataX-> DataX->DataX-> DataX->Property)
        , testProperty "DataY"
            (restrictInner1D   :: DataY-> DataY->DataY-> DataY->Property)
        , testProperty "NominalDiffTime"
            (restrictInner1D :: NominalDiffTime->NominalDiffTime
                                ->NominalDiffTime->NominalDiffTime->Property)
        ]
      , testGroup "restrictLeft1D"
        [ testProperty "Double"
            (restrictLeft1D :: Double->Double->Double->Double->Property)
        , testProperty "ScreenX"
            (restrictLeft1D :: ScreenX->ScreenX->ScreenX->ScreenX->Property)
        , testProperty "ScreenY"
            (restrictLeft1D :: ScreenY->ScreenY->ScreenY->ScreenY->Property)
        , testProperty "CanvasX"
            (restrictLeft1D :: CanvasX->CanvasX->CanvasX->CanvasX->Property)
        , testProperty "CanvasY"
            (restrictLeft1D :: CanvasY->CanvasY->CanvasY->CanvasY->Property)
        , testProperty "DataX"
            (restrictLeft1D :: DataX->DataX->DataX->DataX->Property)
        , testProperty "DataY"
            (restrictLeft1D :: DataY->DataY->DataY->DataY->Property)
        , testProperty "NominalDiffTime"
            (restrictLeft1D :: NominalDiffTime->NominalDiffTime
                               ->NominalDiffTime->NominalDiffTime->Property)
        ]
      , testGroup "restrictMiss1D"
        [ testProperty "Double"
            (restrictMiss1D :: Double->Double->Double->Double->Property)
        , testProperty "ScreenX"
            (restrictMiss1D :: ScreenX->ScreenX->ScreenX->ScreenX->Property)
        , testProperty "ScreenY"
            (restrictMiss1D :: ScreenY->ScreenY->ScreenY->ScreenY->Property)
        , testProperty "CanvasX"
            (restrictMiss1D :: CanvasX->CanvasX->CanvasX->CanvasX->Property)
        , testProperty "CanvasY"
            (restrictMiss1D :: CanvasY->CanvasY->CanvasY->CanvasY->Property)
        , testProperty "DataX"
            (restrictMiss1D :: DataX->DataX->DataX->DataX->Property)
        , testProperty "DataY"
            (restrictMiss1D :: DataY->DataY->DataY->DataY->Property)
        ]
      , testGroup "RestrictNeg"
        [ testProperty "Double"
            (restrictRight1D :: Double->Double->Double->Double->Property)
        , testProperty "ScreenX"
            (restrictRight1D :: ScreenX->ScreenX->ScreenX->ScreenX->Property)
        , testProperty "ScreenY"
            (restrictRight1D :: ScreenY->ScreenY->ScreenY->ScreenY->Property)
        , testProperty "CanvasX"
            (restrictRight1D :: CanvasX->CanvasX->CanvasX->CanvasX->Property)
        , testProperty "CanvasY"
            (restrictRight1D :: CanvasY->CanvasY->CanvasY->CanvasY->Property)
        , testProperty "DataX"
            (restrictRight1D :: DataX->DataX->DataX->DataX->Property)
        , testProperty "DataY"
            (restrictRight1D :: DataY->DataY->DataY->DataY->Property)
        , testProperty "NominalDiffTime"
            (restrictRight1D :: NominalDiffTime->NominalDiffTime
                                ->NominalDiffTime->NominalDiffTime->Property)
        ]
      , testGroup "restrictNeg"
        [ testProperty "Double"
            (restrictNeg :: Range Double->Range Double->Bool)
        , testProperty "ScreenX"
            (restrictNeg :: Range ScreenX->Range ScreenX->Bool)
        , testProperty "ScreenY"
            (restrictNeg :: Range ScreenY->Range ScreenY->Bool)
        , testProperty "CanvasX"
            (restrictNeg :: Range CanvasX->Range CanvasX->Bool)
        , testProperty "CanvasY"
            (restrictNeg :: Range CanvasY->Range CanvasY->Bool)
        , testProperty "DataX"
            (restrictNeg :: Range DataX->Range DataX->Bool)
        , testProperty "DataY"
            (restrictNeg :: Range DataY->Range DataY->Bool)
        , testProperty "NominalDiffTime"
            (restrictNeg :: Range NominalDiffTime->Range NominalDiffTime->Bool)
        ]
      , testGroup "restrict2D"
        [ testProperty "Double/Double"
            (restrictD2V :: Range Double->Range Double->Range Double
                            ->Range Double->Bool)
        , testProperty "Screen X/Y" (restrictD2V ::
            Range ScreenX->Range ScreenX->Range ScreenY->Range ScreenY->Bool)
        , testProperty "Canvas X/Y" (restrictD2V ::
            Range CanvasX->Range CanvasX->Range CanvasY->Range CanvasY->Bool)
        , testProperty "Data X/Y" (restrictD2V ::
            Range DataX->Range DataX->Range DataY->Range DataY->Bool)
        , testProperty "Double/NominalDiffTime" (restrictD2V ::
            Range Double->Range Double->Range NominalDiffTime
            ->Range NominalDiffTime->Bool)
        ]
      ]
      , testGroup "viewRange"
        [ testGroup "identity"
          [ testProperty "Double" (viewRangeIdentity :: Range Double->Bool)
          , testProperty "DataX" (viewRangeIdentity :: Range DataX ->Bool)
          , testProperty "UTCTime" (viewRangeIdentity :: Range UTCTime ->Bool)
          ]
        , testGroup "null"
          [ testProperty "Double" (viewRangeNull :: ScreenX->Range Double->Bool)
          , testProperty "DataX" (viewRangeNull :: ScreenX->Range DataX->Bool)
          , testProperty "UTCTime" (viewRangeNull
              :: ScreenX->Range UTCTime->Bool)
          ]
        ]
    ]
