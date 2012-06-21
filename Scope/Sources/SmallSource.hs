{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------

{- |
--   Module      : Scope.Sources.SmallSource
--   Copyright   : Conrad Parker, John Lato
--   License     : BSD3-style (see LICENSE)
--
--   Maintainer  : Conrad Parker
--   <conrad@metadecks.org>
--   Stability   : unstable
--   Portability : unknown
--
--   Create Scope Sources from small (in-memory) data sets
-}
----------------------------------------------------------------------

module Scope.Sources.SmallSource
(
-- * List sources
  listSource
, listSourceAsc
, listSourceDefault
, listSourceDefaultAsc
-- * Vector sources
, vectorSource
, vectorSourceAsc
, vectorSourceDefault
, vectorSourceDefaultAsc
, ivectorSource
, nvectorSource
) where

import Scope.Types

import qualified Data.Vector.Generic as V
import qualified Data.Vector.Unboxed as U

import Data.List (sortBy)
import Data.Ord (comparing)

-- | Create a @Source@ from a list of tuples.  It is an error to use
-- this function with an empty input list.
listSource :: (U.Unbox a, U.Unbox b, Ord a, Ord b)
           => [(a, b)]
           -> Source a b
listSource = listSourceDefault (error errMsg) (error errMsg)
  where
    errMsg = "Scope.Sources.listSource: called with empty input list.  Maybe try Scope.Sources.listSourceDefault?"

-- | Create a @Source@ from a list of tuples.  It is an error to use
-- this function with an empty input list.
--
-- The input list should be ordered with the 'a' values increasing.
listSourceAsc :: (U.Unbox a, U.Unbox b, Ord a, Ord b)
              => [(a, b)]
              -> Source a b
listSourceAsc = listSourceDefaultAsc (error errMsg) (error errMsg)
  where errMsg = "Scope.Sources.listSourceAsc: called with empty input list.  Maybe try Scope.Sources.listSourceDefaultAsc?"

-- | Create a @Source@ from a list of tuples, specifying default values
-- for the data extent.
--
-- The input list should be ordered with the 'a' values increasing.
listSourceDefault
  :: (U.Unbox a, U.Unbox b, Ord a, Ord b)
  => a
  -> b
  -> [(a, b)]
  -> Source a b
listSourceDefault aDef bDef =
    vectorSourceDefaultAsc aDef bDef . U.fromList . sortBy (comparing fst)

-- | Create a @Source@ from a list of tuples, specifying default values
-- for the data extent.
--
-- The input list should be ordered with the 'a' values increasing.
listSourceDefaultAsc
  :: (U.Unbox a, U.Unbox b, Ord a, Ord b)
  => a
  -> b
  -> [(a, b)]
  -> Source a b
listSourceDefaultAsc aDef bDef = vectorSourceDefaultAsc aDef bDef . U.fromList

-- -----------------------------------------------------------------

-- | Create a @Source@ from a vector of tuples.  It is an error to use
-- this function with an empty input vector.
vectorSource
  :: (U.Unbox a, U.Unbox b, Ord a, Ord b, V.Vector v (a,b))
  => v (a, b)
  -> Source a b
vectorSource v = vectorSourceDefaultAsc (error errMsg) (error errMsg)
                 . U.fromListN vLen
                 . sortBy (comparing fst)
                 $ V.toList v
  where
    vLen = V.length v
    errMsg = "Scope.Sources.vectorSource: called with empty input vector.  Maybe try Scope.Sources.vectorSourceDefault?"

-- | Create a @Source@ from a vector of tuples.  It is an error to use
-- this function with an empty input vector.
--
-- The input vector should be ordered with the 'a' values increasing.
vectorSourceAsc
  :: (U.Unbox a, U.Unbox b, Ord a, Ord b, V.Vector v (a,b))
  => v (a, b)
  -> Source a b
vectorSourceAsc = vectorSourceDefaultAsc (error errMsg) (error errMsg) . V.convert
  where errMsg = "Scope.Sources.vectorSourceAsc: called with empty input vector.  Maybe try Scope.Sources.vectorSourceDefaultAsc?"

-- | Create a @Source@ from a vector of tuples, specifying default values
-- for the data extent.
--
-- The input vector should be ordered with the 'a' values increasing.
vectorSourceDefault
  :: (U.Unbox a, U.Unbox b, Ord a, Ord b, V.Vector v (a,b))
  => a
  -> b
  -> v (a, b)
  -> Source a b
vectorSourceDefault aDef bDef v =
    vectorSourceDefaultAsc aDef bDef . U.fromListN vLen . sortBy (comparing fst)
    $ V.toList v
  where
    vLen = V.length v

-- | Create a @Source@ from a vector of tuples, specifying default values
-- for the data extent.
--
-- The input vector should be ordered with the 'a' values increasing.
vectorSourceDefaultAsc
  :: (U.Unbox a, U.Unbox b, Ord a, Ord b)
  => a
  -> b
  -> U.Vector (a, b)
  -> Source a b
vectorSourceDefaultAsc aDef bDef v = Source genExtent genProvider
  where
    (xMin,xMax,yMin,yMax) = case (U.length v) of
        0 -> (aDef,aDef,bDef,bDef)
        _ -> let (x0,y0) = v U.! 0
                 f (!xmin,!xmax,!ymin,!ymax) (x,y) = (min xmin x, max xmax x
                                                     ,min ymin y, max ymax y)
             in U.foldl' f (x0,x0,y0,y0) v
    xExtent = fromBoundsC xMin xMax
    yExtent = fromBoundsC yMin yMax
    sliceR xRng  = let (lo,hi) = toBounds xRng
                   in U.takeWhile ((<= hi) . fst) . U.dropWhile ((< lo) . fst)
    genExtent = return (xExtent, yExtent)
    genProvider All = return $ \_hint xRng -> return (sliceR xRng v)
    genProvider Sample = return $ \(Hint hint) xRng -> do
        let factor = vLen `div` (max 1 hint)
            v'   = sliceR xRng v
            vLen = U.length v'
        -- don't bother subsampling unless there's a non-negligeable amount of
        -- data
        if factor > 1 && vLen > 20
            then let (newLen,extra) = vLen `divMod` factor
                 in if extra == 0
                      then return $ U.generate newLen (\ix -> v' V.! (ix*factor))
                      else return $ U.generate newLen (\ix -> v' V.! (ix*factor))
                                     `U.snoc` U.last v'
            else return v'
    genProvider (Fold m0 mApp) = return $ \(Hint hint) xRng -> do
        let factor = vLen `div` (max 1 hint)
            v' = sliceR xRng v
            vLen = U.length v'
        -- don't bother subsampling unless there's a non-negligeable amount of
        -- data
        if factor > 1 && vLen > 20
            then let (newLen,extra) = vLen `divMod` factor
                     sampf vec len ix = U.foldl' mApp m0
                                        $ U.slice (ix*factor) len vec
                 in if extra == 0
                      then return $ U.generate newLen (sampf v' factor)
                      else return $ U.generate newLen (sampf v' factor)
                                     `U.snoc` sampf v' extra newLen
            else return v'

-- | Create a @Source@ using each element's index position as the X
-- coordinate
ivectorSource :: (U.Unbox a, V.Vector v a, Ord a)
              => v a -> Source Int a
ivectorSource = vectorSourceAsc . U.indexed . V.convert
{-# INLINE ivectorSource #-}

-- | Create a @Source@ with the X-axis normalized to the range [0,1]
nvectorSource :: (V.Vector v a, U.Unbox a, Ord a)
              => v a -> Source Double a
nvectorSource = vectorSourceAsc . indexD . V.convert
{-# INLINE nvectorSource #-}

indexD :: (U.Unbox a) => U.Vector a -> U.Vector (Double, a)
indexD vec = U.imap (\i x -> (fromIntegral i / denom, x)) $ vec
  where
    denom = fromIntegral $ U.length vec - 1
{-# INLINE indexD #-}
