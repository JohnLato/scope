
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
-- |
-- Module      : Scope.View
-- Copyright   : Conrad Parker
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Conrad Parker <conrad@metadecks.org>
-- Stability   : unstable
-- Portability : unknown
--
-- Functions for dealing with Views
----------------------------------------------------------------------

module Scope.View (
    -- * Motion
      viewAlign
    , viewMoveStart
    , viewMoveEnd
    , viewMoveLeft
    , viewMoveRight
    , viewMoveTo

    -- * Zoom
    , viewZoomIn
    , viewZoomOut
    , viewZoomInOn
    , viewZoomOutOn

    -- * Button handling
    , viewButtonDown
    , viewButtonMotion
    , viewButtonRelease
) where

import Data.VectorSpace
import Scope.Types

----------------------------------------------------------------------
canvasToData :: View ui -> CanvasX -> DataX
canvasToData View{..} (CanvasX cX) = (rangeStart viewX) ^+^
    (cX *^ snd (toSpan viewX))

----------------------------------------------------------------------


viewSetEnds :: DataX -> DataX -> View ui -> View ui
viewSetEnds x1 x2 = viewSetRange (fromBoundsC x1 x2)

viewSetRange :: Range DataX -> View ui -> View ui
viewSetRange xRng v = v { viewX = xRng }

-- | Align a view so the given DataX appears at CanvasX,
-- preserving the current view width.
viewAlign :: CanvasX -> DataX -> View ui -> View ui
viewAlign (CanvasX cx) (DataX dx) v@View{..} = viewSetRange newX' v
    where
        vW'@(DataX vW) = snd $ toSpan viewX   -- current width of view window
        newX1 = DataX $ max 0 $ dx - (cx * vW)
        newX' = restrictRange01 (fromSpanC newX1 vW')

viewMoveStart :: View ui -> View ui
viewMoveStart = viewAlign (CanvasX 0.0) (DataX 0.0)

viewMoveEnd :: View ui -> View ui
viewMoveEnd = viewAlign (CanvasX 1.0) (DataX 1.0)

viewMoveLeft :: View ui -> View ui
viewMoveLeft v@View{..} = viewAlign (CanvasX 0.0) (rangeStart viewX) v

viewMoveRight :: View ui -> View ui
viewMoveRight v@View{..} = viewAlign (CanvasX 1.0) (rangeStart viewX) v

viewMoveTo :: Double -> View ui -> View ui
viewMoveTo val v@View{..} = viewSetRange newX' v
    where
        newX' = restrictRange01 .
            translateRange (DataX val .-. rangeStart viewX) $
            viewX

viewZoomIn :: Double -> View ui -> View ui
viewZoomIn = viewZoomInOn (CanvasX 0.5)

viewZoomInOn :: CanvasX -> Double -> View ui -> View ui
viewZoomInOn focus mult = viewZoomOutOn focus (1.0/mult)

viewZoomOut :: Double -> View ui -> View ui
viewZoomOut = viewZoomOutOn (CanvasX 0.5)

viewZoomOutOn :: CanvasX -> Double -> View ui -> View ui
viewZoomOutOn focus mult v@View{..} = viewSetRange newX' v
    where
        newX' = restrictRange01 $
            zoomRange focus mult viewX

viewButtonDown :: CanvasX -> View ui -> View ui
viewButtonDown cX v = v { dragDX = Just (canvasToData v cX) }

viewButtonMotion :: CanvasX -> View ui -> View ui
viewButtonMotion cX v@View{..} = case dragDX of
    Just dX -> viewAlign cX dX v'
    Nothing -> v'
    where
        v' = v { pointerX = Just cX }

viewButtonRelease :: View ui -> View ui
viewButtonRelease v = v { dragDX = Nothing}

----------------------------------------------------------------------
