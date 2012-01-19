{-# OPTIONS -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
{- |
   Module      : Scope.Numeric.IEEE754
   Copyright   : Conrad Parker
   License     : BSD3-style (see LICENSE)

   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown

   Scope plotting functions
-}

module Scope.Numeric.IEEE754 (
) where

import Control.Arrow (second)
import Data.Maybe (fromJust)
import Data.ZoomCache
import Data.ZoomCache.Numeric

import Scope.Types hiding (b)

----------------------------------------------------------------------

instance ScopePlot Double where
    rawLayerPlot = rawLayerPlotListDouble
    summaryLayerPlot = summaryLayerPlotListDouble

----------------------------------------------------------------------
-- Raw data

rawLayerPlotListDouble :: Double -> RGB -> LayerPlot (TimeStamp, [Double])
rawLayerPlotListDouble maxRange _rgb =
    LayerFold (plotRawListDouble maxRange) plotRawListInitDouble Nothing

plotRawListInitDouble :: [DrawLayer]
plotRawListInitDouble = repeat []

plotRawListDouble :: Double -> LayerFoldFunc (TimeStamp, [Double]) (Maybe [Double])
plotRawListDouble yRange x w Nothing (ts, ys) = plotRawListDouble yRange x w (Just ys) (ts, ys)
plotRawListDouble yRange x w (Just ys0) (ts, ys) =
    second Just $ foldl c ([], []) $ map f (zip3 (map yFunc [0..]) ys0 ys)
    where
        c :: ([a], [b]) -> ([a], b) -> ([a], [b])
        c (ds0, ss) (ds, s) = (ds0++ds, ss++[s])

        l = length ys
        yStep = 2.0 / fromIntegral l
        yFunc n v = (-1.0) + (n * yStep) + ((0.5) * yStep) + (v * yStep / yRange)
        f :: ((Double -> Double), Double, Double) -> ([DrawLayer], Double)
        f (y, s0, s) = second fromJust $ plotRaw1Double y x w (Just s0) (ts, s)

plotRaw1Double :: (Double -> Double) -> LayerFoldFunc (TimeStamp, Double) (Maybe Double)
plotRaw1Double f x w Nothing (ts, y) = plotRaw1Double f x w (Just y) (ts, y)
plotRaw1Double f x w (Just y0) (_ts, y) = (cmds, Just y')
    where
        cmds =
            [ [ MoveTo (x,   y0)
              , LineTo (x+w, y')
              ]
            ]
        y' = f y

----------------------------------------------------------------------
-- Summary data

summaryLayerPlotListDouble :: Double -> RGB -> LayerPlot [Summary Double]
summaryLayerPlotListDouble maxRange rgb =
    LayerFold (plotSummaryListDouble maxRange) (plotSummaryListInitDouble rgb) Nothing

plotSummaryListInitDouble :: RGB -> [DrawLayer]
plotSummaryListInitDouble (r, g, b) = concat $ repeat
    [ [ SetRGBA r g b 0.3 ]
    , [ SetRGB (r*0.6) (g*0.6) (b*0.6) ]
    ]

plotSummaryListDouble :: Double
                      -> LayerFoldFunc [Summary Double] (Maybe [Summary Double])
plotSummaryListDouble dYRange x w Nothing ss =
    plotSummaryListDouble dYRange x w (Just ss) ss
plotSummaryListDouble dYRange x w (Just ss0) ss = do
    second Just $ foldl c ([], []) $ map f (zip3 (map yFunc [0..]) ss0 ss)
    where
        c :: ([a], [b]) -> ([a], b) -> ([a], [b])
        c (ds0, sss) (ds, s) = (ds0++ds, sss++[s])

        l = length ss
        yStep = 2.0 / fromIntegral l
        yFunc n v = (-1.0) + (n * yStep) + ((0.5) * yStep) + (v * yStep / dYRange)
        f :: ((Double -> Double), Summary Double, Summary Double) -> ([DrawLayer], Summary Double)
        f (y, s0, s) = second fromJust $ plotSummary1Double y x w (Just s0) s

-- | Plot one numeric summary
plotSummary1Double :: (Double -> Double)
                   -> LayerFoldFunc (Summary Double) (Maybe (Summary Double))
plotSummary1Double y x w Nothing s =
    plotSummary1Double y x w (Just s) s
plotSummary1Double y x w (Just s0) s = (cmds, Just s)
    where
        cmds =
            [ [ FillPoly [ (x,     y (numMax sd0))
                         , ((x+w), y (numMax sd))
                         , ((x+w), y (numMin sd))
                         , (x,     y (numMin sd0))
                         ]
              ]
            , [ MoveTo (x,     y (numAvg sd0))
              , LineTo ((x+w), y (numAvg sd))
              ]
            ]
        sd0 = summaryData s0
        sd = summaryData s
