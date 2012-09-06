{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
{- |
   Module      : Scope.Types
   Copyright   : Conrad Parker
   License     : BSD3-style (see LICENSE)

   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown

   Scope types and interfaces

   The coordinate system:

@
           CanvasX 0.0                       CanvasX 1.0    DataX 1.0
              |                                 |              |
 DataX 0.0    V                                 V              V
    |
    V          ---------------------------------   <- CanvasY -1.0
              |                                 |
    +---------+---------------------------------+--------------+
    |         |                                 |              |
    |         |                                 |              |
    +---------+---------------------------------+--------------+
              |                                 |
               ---------------------------------   <- CanvasY -1.0
@

-}
----------------------------------------------------------------------

module Scope.Types (
    -- * Coordinates
      Coordinate(..)
    , ScreenX(..)
    , ScreenY(..)
    , CanvasX(..)
    , CanvasY(..)
    , DataX(..)
    , DataY(..)

    , ScreenV
    , CanvasV
    , DataV
    , ScreenP
    , CanvasP
    , DataP

    , Transform(..)
    -- , mkTransform
    -- , mkTSDataTransform

    , translateRange
    , unionBounds'm
    , unionRange
    , restrictRange
    , restrictRange01
    , zoomRange
    , viewRange

    , extentX
    , extentY

    -- * Data Sources
    , Source (..)
    , Scaling (..)
    , Hint (..)

    -- * Drawings
    , Plot (..)
    , PlotInfo (..)
    , Layer (..)
    , emptyPlot
    , mapPlot
    , emptyPlotInfo
    
    -- * Drawing commands
    , Renderer (..)
    , Tick (..)


    -- * Scope
    -- , ScopeFile(..)
    , Scope(..)
    -- , scopeNew
    -- , scopeClose
    -- , scopeUpdate
    , scopeModifyView

    -- * Views
    , View(..)

    -- * Layers
    -- , Layer(..)
    -- , LayerExtents(..)
    -- , LayerPlot(..)
    -- , LayerMapFunc
    -- , LayerFoldFunc
    -- , ScopeLayer(..)
    -- * Reexports
    , module V
) where

import Control.Applicative (Applicative(..), (<$>))
import Control.Arrow (first, (***))

import Data.List (zipWith4)
import Data.Data (Data, Typeable)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Time.Clock

import qualified Data.Vector.Unboxed as U

import Diagrams.Prelude hiding (view,translate,transform, extentX, extentY)
import qualified Diagrams.Prelude as D
import Data.Basis             as V
import Data.RangeSpace        as V
import Data.VectorSpace       as V
import Data.AffineSpace       as V
import Data.AffineSpace.Point as V

----------------------------------------------------------------------

data Transform a = Transform { m :: Double, b :: a }

-- this might disappear?
class Coordinate a where
    fromDouble :: Double -> a
    translate :: a -> Diff a -> a

    transform :: Transform a -> a -> a

newtype ScreenX = ScreenX Double
    deriving (Num, Fractional, Floating, Eq, Ord, Show, Data, Typeable)
newtype ScreenY = ScreenY Double
    deriving (Num, Fractional, Floating, Eq, Ord, Show, Data, Typeable)
newtype CanvasX = CanvasX Double
    deriving (Num, Fractional, Floating, Eq, Ord, Show, Data, Typeable)
newtype CanvasY = CanvasY Double
    deriving (Num, Fractional, Floating, Eq, Ord, Show, Data, Typeable)
newtype DataX   = DataX { unDataX :: Double }
    deriving (Num, Fractional, Floating, Eq, Ord, Show, Data, Typeable)
newtype DataY   = DataY { unDataY :: Double }
    deriving (Num, Fractional, Floating, Eq, Ord, Show, Data, Typeable)

deriving instance AdditiveGroup ScreenX
deriving instance AdditiveGroup ScreenY
deriving instance AdditiveGroup CanvasX
deriving instance AdditiveGroup CanvasY
deriving instance AdditiveGroup DataX
deriving instance AdditiveGroup DataY

instance AdditiveGroup NominalDiffTime where
    zeroV = 0
    (^+^) = (+)
    negateV = negate

instance AffineSpace ScreenX where
    type Diff ScreenX = ScreenX
    (ScreenX l) .-. (ScreenX r) = ScreenX (l .-. r)
    (ScreenX l) .+^ (ScreenX r) = ScreenX (l .+^ r)

instance AffineSpace ScreenY where
    type Diff ScreenY = ScreenY
    (ScreenY l) .-. (ScreenY r) = ScreenY (l .-. r)
    (ScreenY l) .+^ (ScreenY r) = ScreenY (l .+^ r)

instance AffineSpace CanvasX where
    type Diff CanvasX = CanvasX
    (CanvasX l) .-. (CanvasX r) = CanvasX (l .-. r)
    (CanvasX l) .+^ (CanvasX r) = CanvasX (l .+^ r)

instance AffineSpace CanvasY where
    type Diff CanvasY = CanvasY
    (CanvasY l) .-. (CanvasY r) = CanvasY (l .-. r)
    (CanvasY l) .+^ (CanvasY r) = CanvasY (l .+^ r)

instance AffineSpace DataX where
    type Diff DataX = DataX
    (DataX l) .-. (DataX r) = DataX (l .-. r)
    (DataX l) .+^ (DataX r) = DataX (l .+^ r)

instance AffineSpace DataY where
    type Diff DataY = DataY
    (DataY l) .-. (DataY r) = DataY (l .-. r)
    (DataY l) .+^ (DataY r) = DataY (l .+^ r)

instance AffineSpace UTCTime where
    type Diff UTCTime = NominalDiffTime
    (.-.) = diffUTCTime
    (.+^) = flip addUTCTime

instance AffineSpace NominalDiffTime where
    type Diff NominalDiffTime = NominalDiffTime
    (.-.) = (-)
    (.+^) = (+)

instance VectorSpace ScreenX where
    type Scalar ScreenX = Double
    l *^ (ScreenX r) = ScreenX (l *^ r)

instance VectorSpace ScreenY where
    type Scalar ScreenY = Double
    l *^ (ScreenY r) = ScreenY (l *^ r)

instance VectorSpace CanvasX where
    type Scalar CanvasX = Double
    l *^ (CanvasX r) = CanvasX (l *^ r)

instance VectorSpace CanvasY where
    type Scalar CanvasY = Double
    l *^ (CanvasY r) = CanvasY (l *^ r)

instance VectorSpace DataX where
    type Scalar DataX = Double
    l *^ (DataX r) = DataX (l *^ r)

instance VectorSpace DataY where
    type Scalar DataY = Double
    l *^ (DataY r) = DataY (l *^ r)

-- | Having Scalar NominalDiffTime = Double means that scaling
-- and basis decomposition have to go through Rational.
-- Maybe this should be revisited.
instance VectorSpace NominalDiffTime where
    -- Scalar is Double so we can form a basis like
    -- D2V (DataX, NominalDiffTime)
    type Scalar NominalDiffTime = Double
    s *^ difftime = (realToFrac s) * difftime

instance HasBasis ScreenX where
    type Basis ScreenX = ()
    basisValue () = 1
    decompose (ScreenX v) = decompose v
    decompose' (ScreenX v) = decompose' v

instance HasBasis ScreenY where
    type Basis ScreenY = ()
    basisValue () = 1
    decompose (ScreenY v) = decompose v
    decompose' (ScreenY v) = decompose' v

instance HasBasis CanvasX where
    type Basis CanvasX = ()
    basisValue () = 1
    decompose (CanvasX v) = decompose v
    decompose' (CanvasX v) = decompose' v

instance HasBasis CanvasY where
    type Basis CanvasY = ()
    basisValue () = 1
    decompose (CanvasY v) = decompose v
    decompose' (CanvasY v) = decompose' v

instance HasBasis DataX where
    type Basis DataX = ()
    basisValue () = 1
    decompose (DataX v) = decompose v
    decompose' (DataX v) = decompose' v

instance HasBasis DataY where
    type Basis DataY = ()
    basisValue () = 1
    decompose (DataY v) = decompose v
    decompose' (DataY v) = decompose' v

instance HasBasis NominalDiffTime where
    type Basis NominalDiffTime = ()
    basisValue () = 1
    decompose dtime = [((), realToFrac dtime)]
    decompose' dtime () = realToFrac dtime

type ScreenV = D2V ScreenX ScreenY
type CanvasV = D2V CanvasX CanvasY
type DataV   = D2V DataX DataY

type ScreenP = Point ScreenV
type CanvasP = Point CanvasV
type DataP   = Point DataV

instance Coordinate Double where
    fromDouble = id
    translate t x = x + t
    transform Transform{..} x = m * x + b

instance Coordinate ScreenX where
    fromDouble = ScreenX
    translate (ScreenX t) (ScreenX x)  = ScreenX (translate t x)
    transform (Transform m (ScreenX b)) (ScreenX x) =
        ScreenX (transform (Transform m b) x)

instance Coordinate CanvasX where
    fromDouble = CanvasX
    translate (CanvasX t) (CanvasX x)  = CanvasX (translate t x)
    transform (Transform m (CanvasX b)) (CanvasX x) =
        CanvasX (transform (Transform m b) x)

instance Coordinate DataX where
    fromDouble = DataX
    translate (DataX t) (DataX x)  = DataX (translate t x)
    transform (Transform m (DataX b)) (DataX x) =
        DataX (transform (Transform m b) x)

utc0 :: UTCTime
utc0 = UTCTime (toEnum 0) (fromInteger 0)

unionBounds'm :: (Num (Scalar (Diff a)), Ord (Scalar (Diff a)), HasBasis (Diff a)
               , AffineSpace a, Ord a)
            => Maybe (Range a)
            -> Maybe (Range a)
            -> Maybe (Range a)
unionBounds'm a         Nothing   = a
unionBounds'm Nothing   b         = b
unionBounds'm (Just r1) (Just r2) = Just (unionRange r1 r2)

-- | Restrict a window to within a given range
restrictRange :: (Ord a, Coordinate a, AffineSpace a, HasBasis (Diff a)
                 ,Num (Scalar (Diff a)), Eq (Basis (Diff a)), Ord (Scalar (Diff a)))
              => Range a -> Range a -> Range a
restrictRange = maskRange

restrictRange01 :: (Ord a, Coordinate a, AffineSpace a, HasBasis (Diff a)
                   ,Ord (Scalar (Diff a)), Num (Scalar (Diff a)), Eq (Basis (Diff a)))
                => Range a
                -> Range a
restrictRange01 = maskRange $ fromBounds (fromDouble 0.0, fromDouble 1.0)

-- | Translate the range by the given amount on the canvas,
-- scaled by the given multiplier.
zoomRange :: (AdditiveGroup t, VectorSpace (Diff t), AffineSpace t,
              Coordinate t)
          => CanvasX -> Scalar (Diff t) -> Range t -> Range t
zoomRange (CanvasX focus) mult rng =
        fromSpanC (start ^+^ fromDouble focus) (dist ^* mult)
    where
        (start, dist) = toSpan rng

-- | A view into a full extent, as specified by 'Range ScreenX'.
--
-- > viewRange (fromBoundsC 0 1) === id
--
-- To get a 'Range' of the first half of an extent, use
--
-- > viewRange (fromBoundsC 0 0.5) fullExtent
viewRange :: (VectorSpace (Diff t), AffineSpace t, Scalar (Diff t) ~ Double)
          => Range ScreenX -> Range t -> Range t
viewRange slice extent = fromSpanC newstart (width *^ dlength)
  where
    (ScreenX pstart, ScreenX width) = toSpan slice
    (dstart, dlength) = toSpan extent
    newstart = dstart .+^ (pstart *^ dlength)

----------------------------------------------------------------------
-- Data sources

-- | When data is requested from a @Source@, controls the level of detail of
-- the returned data.  @Sample@ samples the underlying data at required points,
-- @Fold@ accumulates a value between points, and @All@ is full detail.
--
-- In some cases the source may ignore a @Scaling@ factor.  In particular, if
-- the underlying data isn't particularly dense, the source may just return
-- @All@ regardless of the specified scaling.
data Scaling d =
    All
  | Sample
  | Fold d (d -> d -> d)

-- | Specify about how much data is to be requested from a source.  Hinting
-- is useful for dense time-series data, so that the datasource can return a
-- pruned view if possible.  The actual pruning implementation is controlled
-- by the 'Scaling' parameter.  Some sources may ignore @Hint@ values entirely.
--
newtype Hint = Hint { unHint :: Int }
  deriving (Eq, Show)

-- | A @Source@ can be run to provide data.
--
-- The 'Scaling' parameter describes how the underlying data should be sampled.
-- The 'Range (ScreenP)' gives the current range of the window for which data
-- should be retrieved (normalized 0-1).
--
data Source sourceX sourceY = Source
   { sourceExtent      :: IO (Range sourceX, Range sourceY)
   , genSourceProvider :: Scaling (sourceX,sourceY)
                          -> IO (Hint
                                -> Range (sourceX)
                                -> IO (U.Vector (sourceX, sourceY)))
   }

----------------------------------------------------------------------

-- | A @PlotInfo@ contains all non-graphical information about a plot.
data PlotInfo = PlotInfo
   { piLegendX   :: String
   , piLegendY   :: String
   , piTitle     :: String
   } deriving (Eq, Show, Ord)

-- | An empty @PlotInfo@.
emptyPlotInfo :: PlotInfo
emptyPlotInfo = PlotInfo "" "" ""

-- | A @Plot@ describes how to generate a diagram from a given data source.
data Plot sourceX sourceY diag = Plot
   { makePlot    :: U.Vector (sourceX,sourceY) -> diag
   , plotInfo    :: PlotInfo
   }

-- | An empty @Plot@.  Creates an empty diagram regardless of input.
emptyPlot :: Monoid diag => Plot a b diag
emptyPlot = Plot (const mempty) emptyPlotInfo

-- Map a function over the data vector passed to a @Plot@.
--
-- Useful for converting from rich data sources to the more limited types
-- available in built-in plots.
mapPlot :: (U.Unbox outX, U.Unbox outY, U.Unbox inX, U.Unbox inY)
        => (inX -> outX)
        -> (inY -> outY)
        -> Plot outX outY diag
        -> Plot inX inY diag
mapPlot xf yf p@Plot{..} = p {makePlot = newPlot}
  where
    newPlot = makePlot . U.map (xf *** yf)

-- | A Layer is a single layer of the final image.  Layers are used for
-- compositing plots.
data Layer diagram = Layer
  { layerPlotInfo :: PlotInfo
  , layerRenderer :: Renderer diagram
  }

----------------------------------------------------------------------
data Renderer diagram = Renderer
    { plotRenderer  :: Hint -> Range DataX -> Range DataY -> IO diagram
    , tickXRenderer :: Range DataX -> IO [Tick DataX]
    , tickYRenderer :: Range DataY -> IO [Tick DataY]
    }

data Tick b = Tick
    { tickLevel :: Int
    , tickLabel :: Maybe String
    , tickValue :: b
    }
    deriving (Eq, Ord, Show, Functor)

data Scope diagram ui = Scope
    { view   :: View ui
    , layers :: [Layer diagram]
    }

data View ui = View
    { viewX  :: Range DataX
    , viewY  :: Range DataY
    , pointerX :: Maybe CanvasX
    , dragDX :: Maybe DataX -- DataX of pointer at drag down
    , viewUI :: ui
    }

scopeModifyView :: (View ui -> View ui) -> Scope diag ui -> Scope diag ui
scopeModifyView f scope = scope{ view = f (view scope) }

addPlot :: Source sourceX sourceY
        -> Plot sourceX sourceY diagram
        -> Scaling (sourceX, sourceY)
        -> Scope diagram ui
        -> IO (Scope diagram ui)
addPlot Source{..} Plot{..} sourceScaling s@Scope{..} = do
    prov <- genSourceProvider sourceScaling
    renderFn <- error "not yet implemented"
    return undefined

-- | A simplified version of 'mkRenderer' for 2-dimensional numeric data
mkRenderer2D :: (InnerSpace sourceX, InnerSpace sourceY
              ,U.Unbox sourceX, U.Unbox sourceY
              ,Scalar sourceX ~ Double, Scalar sourceY ~ Double
              ,Num sourceX, Num sourceY
              ,Backend b R2, Monoid' m)
           => Scaling (sourceX, sourceY)
           -> Source sourceX sourceY
           -> Plot sourceX sourceY (QDiagram b R2 m)
           -> IO (Renderer (QDiagram b R2 m))
mkRenderer2D sourceScaling Source{..} Plot{..} = do
    mkSource <- genSourceProvider sourceScaling
    let tickX = undefined
        tickY = undefined
        xProj = 1
        yProj = 1
    let plotRenderer hint xRng yRng = do
        (xRngAbs, yRngAbs) <- sourceExtent
        let (xProjMin,xProjMax) = toBounds $ (<.> xProj) <$> xRngAbs
            (yProjMin,yProjMax) = toBounds $ (<.> yProj) <$> yRngAbs

            xReqS = (lerp xProjMin xProjMax . unDataX) <$> xRng
            xReq = (*^ xProj) <$> xReqS
            (xReqMin,xReqMax) = toBounds xReqS

            yReqS = (lerp yProjMin yProjMax . unDataY) <$> yRng
            (yReqMin,yReqMax) = toBounds yReqS

        datavec <- mkSource hint xReq

        let (!xMin,!xMax,!yMin,!yMax) = case U.length datavec of
                0 -> let x = xReqMin
                         y = yProjMin
                     in (x,x,y,y)
                1 -> let (x,y) = datavec U.! 0
                         xAdj = x <.> xProj
                         yAdj = y <.> yProj
                     in (xAdj,xAdj,yAdj,yAdj)
                _ -> let (xh,yh) = datavec U.! 0
                         xh' = xh <.> xProj
                         yh' = yh <.> yProj
                         f (!x0,!x1,!y0,!y1) (x,y) = (min x0 (x <.> xProj)
                                                     ,max x1 (x <.> xProj)
                                                     ,min y0 (y <.> yProj)
                                                     ,max y1 (y <.> yProj))
                     in U.foldl' f (xh',xh',yh',yh') datavec

        let rawDiag    = makePlot datavec
            -- TODO: is it necessary to scale by the returned diagram size?
            -- maybe can just use the requested/received diffs as struts
            -- requires convention as to diagram size though.
            (dWidth, dHeight)  = size2D rawDiag

            xProjScaleFac = let xdiff' = xMax - xMin
                            in if dWidth > 0 && xdiff' > 0 then dWidth / xdiff' else 1
            yProjScaleFac = let ydiff' = yMax - yMin
                            in if dHeight > 0 && ydiff' > 0 then dHeight / ydiff' else 1

            xMinDiff = xMin - xReqMin
            xMaxDiff = xReqMax - xMax
            yMinDiff = yMin - yReqMin
            yMaxDiff = yReqMax - yMax

            xstrut dif = if dif > 0 then strutX (dif * xProjScaleFac) else mempty
            ystrut dif = if dif > 0 then strutY (dif * yProjScaleFac) else mempty

            paddedDiag = ystrut yMaxDiff
                         ===
                         (xstrut xMinDiff ||| makePlot datavec ||| xstrut xMaxDiff)
                         ===
                         ystrut yMinDiff
            clipMin = p2 (xReqMin, yReqMin)
            clipV   = p2 (xReqMax, yReqMax) .-. clipMin
        return $ D.view clipMin clipV paddedDiag
    return $ Renderer plotRenderer tickX tickY

-- | create a 'Renderer' for the provided 'Source' and 'Plot'
--
-- A low-level function that typically wouldn't be called by user code.
mkRenderer :: (InnerSpace sourceX, InnerSpace sourceY
              ,U.Unbox sourceX, U.Unbox sourceY
              ,Scalar sourceX ~ Double, Scalar sourceY ~ Double
              ,Backend b R2, Monoid' m)
           => IO (sourceX,sourceY)     -- unit vectors in the direction of the x/y views
           -> Scaling (sourceX, sourceY)
           -> Source sourceX sourceY
           -> Plot sourceX sourceY (QDiagram b R2 m)
           -> IO (Renderer (QDiagram b R2 m))
mkRenderer proj sourceScaling Source{..} Plot{..} = do
    mkSource <- genSourceProvider sourceScaling
    let tickX = undefined
        tickY = undefined
    let plotRenderer hint xRng yRng = do
        (xRngAbs, yRngAbs) <- sourceExtent
        (xProj,yProj) <- proj
        let (xProjMin,xProjMax) = toBounds $ (<.> xProj) <$> xRngAbs
            (yProjMin,yProjMax) = toBounds $ (<.> yProj) <$> yRngAbs

            -- calculate the projection of the available min/max x-values
            -- onto the viewable x-axis, to determine the min/max x-values we
            -- should request from the data source.

            -- scalar projection of available extent onto view-axis vector
            xReqS = (lerp xProjMin xProjMax . unDataX) <$> xRng
            xReq = (*^ xProj) <$> xReqS
            (xReqMin,xReqMax) = toBounds xReqS

            yReqS = (lerp yProjMin yProjMax . unDataY) <$> yRng
            (yReqMin,yReqMax) = toBounds yReqS

        datavec <- mkSource hint xReq

        let (!xMin,!xMax,!yMin,!yMax) = case U.length datavec of
                0 -> let x = xReqMin
                         y = yProjMin
                     in (x,x,y,y)
                1 -> let (x,y) = datavec U.! 0
                         xAdj = x <.> xProj
                         yAdj = y <.> yProj
                     in (xAdj,xAdj,yAdj,yAdj)
                _ -> let (xh,yh) = datavec U.! 0
                         xh' = xh <.> xProj
                         yh' = yh <.> yProj
                         f (!x0,!x1,!y0,!y1) (x,y) = (min x0 (x <.> xProj)
                                                     ,max x1 (x <.> xProj)
                                                     ,min y0 (y <.> yProj)
                                                     ,max y1 (y <.> yProj))
                     in U.foldl' f (xh',xh',yh',yh') datavec

        let rawDiag    = makePlot datavec
            -- TODO: is it necessary to scale by the returned diagram size?
            -- maybe can just use the requested/received diffs as struts
            -- requires convention as to diagram size though.
            (dWidth, dHeight)  = size2D rawDiag

            xProjScaleFac = let xdiff' = xMax - xMin
                            in if dWidth > 0 && xdiff' > 0 then dWidth / xdiff' else 1
            yProjScaleFac = let ydiff' = yMax - yMin
                            in if dHeight > 0 && ydiff' > 0 then dHeight / ydiff' else 1

            xMinDiff = xMin - xReqMin
            xMaxDiff = xReqMax - xMax
            yMinDiff = yMin - yReqMin
            yMaxDiff = yReqMax - yMax

            xstrut dif = if dif > 0 then strutX (dif * xProjScaleFac) else mempty
            ystrut dif = if dif > 0 then strutY (dif * yProjScaleFac) else mempty

            paddedDiag = ystrut yMaxDiff
                         ===
                         (xstrut xMinDiff ||| makePlot datavec ||| xstrut xMaxDiff)
                         ===
                         ystrut yMinDiff
            clipMin = p2 (xReqMin, yReqMin)
            clipV   = p2 (xReqMax, yReqMax) .-. clipMin
        return $ D.view clipMin clipV paddedDiag
    return $ Renderer plotRenderer tickX tickY

----------------------------------------------------------------------
