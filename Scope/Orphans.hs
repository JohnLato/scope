{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      : Scope.Orphans
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- Orphan instances
----------------------------------------------------------------------

module Scope.Orphans
where

import Scope.Types

import Data.Time.Clock

----------------------------------------------------------------------

instance AdditiveGroup NominalDiffTime where
    zeroV = 0
    (^+^) = (+)
    negateV = negate

-- We want an AffineSpace instance for UTCTime, because
-- then we can use 'Range UTCTime'
instance AffineSpace UTCTime where
    type Diff UTCTime = NominalDiffTime
    (.-.) = diffUTCTime
    (.+^) = flip addUTCTime

-- | Having Scalar NominalDiffTime = Double means that scaling
-- and basis decomposition have to go through Rational.
-- Maybe this should be revisited.
instance VectorSpace NominalDiffTime where
    -- Scalar is Double so we can form a basis like
    -- D2V (DataX, NominalDiffTime)
    type Scalar NominalDiffTime = Double
    s *^ difftime = (realToFrac s) * difftime

instance AffineSpace NominalDiffTime where
    type Diff NominalDiffTime = NominalDiffTime
    (.-.) = (-)
    (.+^) = (+)

instance HasBasis NominalDiffTime where
    type Basis NominalDiffTime = ()
    basisValue () = 1
    decompose dtime = [((), realToFrac dtime)]
    decompose' dtime () = realToFrac dtime

