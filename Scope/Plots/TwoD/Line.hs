{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
{- |
   Module      : Scope.Plots.TwoD.Line
   Copyright   : Conrad Parker, John Lato
   License     : BSD3-style (see LICENSE)

   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown

   Scope built-in plots

-}
----------------------------------------------------------------------

module Scope.Plots.TwoD.Line (
  linePlot
, linePlotD
) where

import Scope.Types
import Diagrams.Prelude
import qualified Data.Vector.Unboxed as V

-- | A line plot for any @Real@ type.
linePlot :: (V.Unbox x, V.Unbox y, Real x, Real y, Renderable (Path R2) b)
         => Plot x y (Diagram b R2)
linePlot = mapPlot realToFrac realToFrac linePlotD

-- | A basic line plot for '(Double,Double)' data.
linePlotD :: (Renderable (Path R2) b) => Plot Double Double (Diagram b R2)
linePlotD = Plot plotter emptyPlotInfo
    where
        plotter = fromVertices . map p2 . V.toList
