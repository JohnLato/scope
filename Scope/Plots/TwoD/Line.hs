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

, filledLinePlot
, filledLinePlotD

, minMeanMaxPlot
) where

import Scope.Types
import Diagrams.Prelude

import qualified Data.Vector.Unboxed as V
import Control.Arrow

-- | A line plot for any @Real@ type.
linePlot :: (V.Unbox x, V.Unbox y, Real x, Real y, Renderable (Path R2) b)
         => Plot x y (Diagram b R2)
linePlot = mapPlot realToFrac realToFrac linePlotD

-- | A basic line plot for '(Double,Double)' data.
linePlotD :: (Renderable (Path R2) b) => Plot Double Double (Diagram b R2)
linePlotD = Plot plotter emptyPlotInfo
    where
        plotter = fromVertices . map p2 . V.toList

filledLinePlot :: (V.Unbox x, V.Unbox y, Real x, Real y, Renderable (Path R2) b)
         => Plot x (y,y) (Diagram b R2)
filledLinePlot = mapPlot realToFrac (realToFrac *** realToFrac) filledLinePlotD

filledLinePlotD :: (Renderable (Path R2) b) => Plot Double (Double,Double) (Diagram b R2)
filledLinePlotD = Plot plotter emptyPlotInfo
    where
        plotter xs = let lo = map (p2 . second fst) $ V.toList xs
                         hi = map (p2 . second snd) $ V.toList xs
                     in stroke . close $ fromVertices (lo ++ reverse hi)

minMeanMaxPlot
    :: (V.Unbox x, V.Unbox y, Real x, Real y, Renderable (Path R2) b)
         => Plot x (y,y,y) (Diagram b R2)
minMeanMaxPlot = Plot plotter emptyPlotInfo
    where
        plot1 = makePlot $ mapPlot id (\(_,b,c) -> (b,c)) filledLinePlot
        plot2 = makePlot $ mapPlot id (\(a,_,_) -> a) linePlot
        -- probably need to strut these out.  Easier if we add them later,
        -- after the data's been normalized.
        plotter = atop <$> plot1 <*> plot2
