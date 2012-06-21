{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Scope.Axes
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- Functions for creating tickmarks and axes
----------------------------------------------------------------------

module Scope.Axes (
-- * Fixed-value ticks
  fitTicks
-- * Arbitrary-position ticks
, enumTicks
-- * Functions
, labelTop
, labelAll
, labelLevels
) where

import Scope.Types

----------------------------------------------------------------------

-- | A sequence of tick marks to fit within a range, with the ticks spaced
-- about 0.
--
-- > fitTicks 1 5 (fromBounds (8,21))
-- will produce ticks at 10,15,20
fitTicks :: (Num a, Ord a, Enum a)
         => Int        -- ^ level
         -> a          -- ^ step size
         -> Range a
         -> [Tick a]
fitTicks lvl step rng = map (Tick lvl Nothing) tvals
  where
    (start,stop) = toBounds rng
    fI = fromIntegral
    tvals = if start >= 0
              then takeWhile (<= stop)
                   . dropWhile (< start)
                   $ [ fI i * step | i <- [0,1..] :: [Int]]
              else takeWhile (<= stop)
                   $ reverse
                        (takeWhile (> start) [ fI i * (-step) | i <- [0,1..] :: [Int]])
                     ++ [ fI i * step | i <- [0,1..] :: [Int]]

----------------------------------------------------------------------

-- | Create a sequence of tick marks to fit a range, from the range start to
-- the range end.
--
-- > enumTicks 1 5 (fromBounds (8,21))
-- will produce ticks at 8,13,18,21
enumTicks :: (Real a, Num a, Ord a, Enum a)
          => Int        -- ^ level
          -> a          -- ^ step size
          -> Range a
          -> [Tick a]
enumTicks lvl step rng = map (Tick lvl Nothing) tvals
  where
    toR = toRational
    (numTicks'::Int,rest) = properFraction (toR (stop - start) / toR step)
    numTicks = max 1 numTicks'
    tvals = [start + (step * fromIntegral i) | i <- [0..numTicks]]
            ++ if rest == 0 then [] else [stop]
    (start,stop) = toBounds rng

----------------------------------------------------------------------

labelTop :: (a -> String) -> [Tick a] -> [Tick a]
labelTop showFn = labelLevels showFn [1]

labelAll :: (a -> String) -> [Tick a] -> [Tick a]
labelAll showFn = map f
  where
    f tk@Tick{..} = tk {tickLabel = Just $ showFn tickValue}

labelLevels :: (a -> String) -> [Int] -> [Tick a] -> [Tick a]
labelLevels showFn lvlToUpdate = map f
  where
    f tk@Tick{..}
        | tickLevel `elem` lvlToUpdate = tk {tickLabel = Just $ showFn tickValue}
        | otherwise = tk

----------------------------------------------------------------------
