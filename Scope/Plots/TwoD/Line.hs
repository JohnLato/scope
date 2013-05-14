{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
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

, filledLinePlot
, filledLinePlotD

, minMeanMaxPlot

, PlotName (..)
) where

import Scope.Types
import Diagrams.Prelude

import qualified Data.Vector.Unboxed as V
import Control.Arrow
import Data.Typeable

-- | A line plot for any @Real@ type.
linePlot :: (V.Unbox x, V.Unbox y, Real x, Real y, Renderable (Path R2) b)
         => Plot x y (Diagram b R2)
linePlot = mapPlot realToFrac realToFrac linePlotD

-- | A basic line plot for '(Double,Double)' data.
linePlotD :: (Renderable (Path R2) b) => Plot Double Double (Diagram b R2)
linePlotD = Plot plotter emptyPlotInfo
    where
        plotter = (:[]) . ("line",) . fromVertices . map p2 . V.toList

filledLinePlot :: (V.Unbox x, V.Unbox y, Real x, Real y, Renderable (Path R2) b)
         => Plot x (y,y) (Diagram b R2)
filledLinePlot = mapPlot realToFrac (realToFrac *** realToFrac) filledLinePlotD

filledLinePlotD :: (Renderable (Path R2) b) => Plot Double (Double,Double) (Diagram b R2)
filledLinePlotD = Plot plotter emptyPlotInfo
    where
        plotter xs = let lo = map (p2 . second fst) $ V.toList xs
                         hi = map (p2 . second snd) $ V.toList xs
                     in (:[]) . ("fill",) . stroke . close $ fromVertices (lo ++ reverse hi)

-- | Return a 'Plot' of the mean line with a min/max fill.  The interior
-- diagrams are named "fill" and "line" so that transforms (e.g. coloring)
-- can be applied separately using 'withName'.
minMeanMaxPlot
    :: (V.Unbox x, V.Unbox y, Real x, Real y, Renderable (Path R2) b)
         => Plot x (y,y,y) (Diagram b R2)
minMeanMaxPlot = Plot plotter emptyPlotInfo
    where
        plot1 = (makePlot $ mapPlot id (\(_,mn,_) -> mn) linePlot)
        plot2 = (makePlot $ mapPlot id (\(a,_,c) -> (a,c)) filledLinePlot)
        -- probably need to strut these out.  Easier if we add them later,
        -- after the data's been normalized.
        -- order matters!  we want the linePlot first so it goes on top.
        plotter = (++) <$> plot1 <*> plot2

data PlotName = Fill | Line deriving (Eq, Show, Ord, Typeable)
instance IsName PlotName
