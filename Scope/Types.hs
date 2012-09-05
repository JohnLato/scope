{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

    , Transform(..)
    , mkTransform
    , mkTSDataTransform

    , unionBounds
    , translateRange
    , unionRange
    , restrictRange
    , restrictRange01
    , zoomRange

    -- * Drawing commands
    , RGB
    , DrawCmd(..)
    , DrawLayer
    , ScopeRender(..)

    , ScopePlot(..)

    , ReadMethods(..)
    , ScopeRead(..)

    -- * Scope
    , ScopeFile(..)
    , Scope(..)
    , scopeNew
    , scopeClose
    , scopeUpdate
    , scopeModifyView

    -- * Views
    , View(..)

    -- * Layers
    , Layer(..)
    , LayerExtents(..)
    , LayerPlot(..)
    , LayerMapFunc
    , LayerFoldFunc
    , ScopeLayer(..)
) where

import Control.Applicative ((<$>))
import Control.Monad.CatchIO
import Control.Monad.Trans (MonadIO)
import Data.Iteratee (Iteratee, Enumeratee)
import Data.List (nub)
import Data.Offset
import Data.Maybe
import Data.RangeSpace ( Range, AffineSpace(..), AdditiveGroup(..), VectorSpace(..)
                       , HasBasis(..), translateRange, unionRange, range, rangeStart
                       , maskRange, toBounds, fromBounds)
import Data.Time.Clock
import Data.ZoomCache
import System.Posix

----------------------------------------------------------------------

data Transform a = Transform { m :: Double, b :: a }

class Coordinate a where
    fromDouble :: Double -> a
    toDouble :: a -> Double

    -- | Distance from to
    distance :: a -> a -> a
    -- | Translate x by
    translate :: a -> a -> a

    transform :: Transform a -> a -> a

newtype ScreenX = ScreenX Double deriving (Eq, Ord, Show, Num)
newtype ScreenY = ScreenY Double deriving (Eq, Ord, Show, Num)
newtype CanvasX = CanvasX Double deriving (Eq, Ord, Show, Num)
newtype CanvasY = CanvasY Double deriving (Eq, Ord, Show, Num)
newtype DataX   = DataX   Double deriving (Eq, Ord, Show, Num)
newtype DataY   = DataY   Double deriving (Eq, Ord, Show, Num)


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

instance AffineSpace TimeStamp where
    type Diff TimeStamp = Double
    l .-. r  = toDouble l - toDouble r
    l .+^ r  = fromDouble (toDouble l + fromDouble r)

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

instance Coordinate Double where
    fromDouble = id
    toDouble = id
    distance x1 x2 = x2 - x1
    translate t x = x + t
    transform Transform{..} x = m * x + b

instance Coordinate ScreenX where
    fromDouble d = ScreenX d
    toDouble (ScreenX d) = d
    distance (ScreenX x1) (ScreenX x2) = ScreenX (distance x1 x2)
    translate (ScreenX t) (ScreenX x)  = ScreenX (translate t x)
    transform (Transform m (ScreenX b)) (ScreenX x) = ScreenX (transform (Transform m b) x)

instance Coordinate CanvasX where
    fromDouble d = CanvasX d
    toDouble (CanvasX d) = d
    distance (CanvasX x1) (CanvasX x2) = CanvasX (distance x1 x2)
    translate (CanvasX t) (CanvasX x)  = CanvasX (translate t x)
    transform (Transform m (CanvasX b)) (CanvasX x) = CanvasX (transform (Transform m b) x)

instance Coordinate DataX where
    fromDouble d = DataX d
    toDouble (DataX d) = d
    distance (DataX x1) (DataX x2) = DataX (distance x1 x2)
    translate (DataX t) (DataX x)  = DataX (translate t x)
    transform (Transform m (DataX b)) (DataX x) = DataX (transform (Transform m b) x)

instance Coordinate TimeStamp where
    fromDouble d = TS d
    toDouble (TS d) = d
    distance (TS x1) (TS x2) = TS (distance x1 x2)
    translate (TS t) (TS x)  = TS (translate t x)
    transform (Transform m (TS b)) (TS x) = TS (transform (Transform m b) x)

instance Coordinate UTCTime where
    fromDouble d = addUTCTime (fromRational . toRational $ d) utc0
    toDouble u = fromRational . toRational $ diffUTCTime u utc0
    distance u1 u2 = fromDouble (distance (toDouble u1) (toDouble u2))
    translate t u = fromDouble (translate (toDouble t) (toDouble u))
    transform (Transform m b) x = fromDouble (transform (Transform m (toDouble b)) (toDouble x))


utc0 :: UTCTime
utc0 = UTCTime (toEnum 0) (fromInteger 0)

unionBounds :: (Num (Scalar (Diff a)), Ord (Scalar (Diff a)), HasBasis (Diff a)
               , AffineSpace a, Ord a)
            => Maybe (Range a)
            -> Maybe (Range a)
            -> Maybe (Range a)
unionBounds a         Nothing   = a
unionBounds Nothing   b         = b
unionBounds (Just r1) (Just r2) = Just (unionRange r1 r2)

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

zoomRange :: (Coordinate a, Ord a) => CanvasX -> Double -> Range a -> Range a
zoomRange (CanvasX focus) mult x = fromBounds (translate off1 x1, translate off2 x2)
    where
        (x1,x2) = toBounds x
        off1 = fromDouble $ (oldW - newW) * focus
        off2 = fromDouble $ (newW - oldW) * (1.0 - focus)
        oldW = toDouble $ distance x1 x2
        newW = min 1.0 (oldW * mult)

mkTransform :: (Coordinate a, Coordinate (Diff a), Ord a, AffineSpace a)
            => Range a
            -> Range a
            -> Transform a
mkTransform old new = Transform m b
    where
        oldW = range old
        newW = range new
        m = toDouble oldW / toDouble newW
        b = distance (rangeStart new) (rangeStart old)

mkTSDataTransform :: Range TimeStamp -> Range TimeStamp -> Transform DataX
mkTSDataTransform old new = Transform m b
    where
        oldW = range old
        newW = range new
        stDiff = distance (rangeStart new) (rangeStart old)
        m = toDouble oldW / toDouble newW
        b = fromDouble $ toDouble stDiff / toDouble newW

mkUTCDataTransform :: Range UTCTime -> Range UTCTime -> Transform DataX
mkUTCDataTransform old new = Transform m b
    where
        oldW = range old
        newW = range new
        stDiff = rangeStart old .-. rangeStart new
        m = fromRational . toRational $ oldW / newW
        b = fromDouble . fromRational . toRational $ stDiff / newW

----------------------------------------------------------------------

type RGB = (Double, Double, Double)

data DrawCmd =
      SetRGB   Double Double Double
    | SetRGBA  Double Double Double Double
    | MoveTo   (Double, Double)
    | LineTo   (Double, Double)
    | FillPoly [(Double, Double)]

----------------------------------------------------------------------

class (Functor m, MonadCatchIO m) => ScopeRender m where
    renderCmds :: [DrawCmd] -> m ()

instance ScopeRender IO where
    renderCmds = const (return ())

----------------------------------------------------------------------

data ScopeFile = ScopeFile
    { filename :: FilePath
    , fd       :: Fd
    , scopeCF  :: CacheFile
    }

----------------------------------------------------------------------

type DrawLayer = [DrawCmd]

-- | A layer plotting function which is just given the x position and x width
-- to render the data value of type 'a' into.
type LayerMapFunc a = Double -> Double -> a -> [DrawLayer]

-- | A layer plotting function which is given the x position and x width,
-- and a previously returned value of type 'b'
type LayerFoldFunc a b = Double -> Double -> b -> a -> ([DrawLayer], b)

data LayerPlot a = LayerMap (LayerMapFunc a) [DrawLayer]
                 | forall b . LayerFold (LayerFoldFunc a b) [DrawLayer] b

data LayerExtents = LayerExtents
    { startTime :: TimeStamp
    , endTime :: TimeStamp
    , rangeY :: Double -- XXX: use minY, maxY to allow asymmetric data
    }

data Layer a = Layer
    { layerFile :: ScopeFile
    , layerTrackNo :: TrackNo
    , layerBaseUTC :: Maybe UTCTime
    , layerExtents :: LayerExtents
    , convEnee :: forall m . (Functor m, Monad m) => Enumeratee [Offset Block] [a] m ()
    , plotter :: LayerPlot a
    }

data ScopeLayer = forall a . Timestampable a => ScopeLayer (Layer a)

----------------------------------------------------------------------

class ScopePlot a where
    rawLayerPlot :: LayerExtents -> RGB -> LayerPlot (TimeStamp, [a])
    summaryLayerPlot :: LayerExtents -> RGB -> LayerPlot [Summary a]

----------------------------------------------------------------------

data ReadMethods a = ReadMethods
    { readIdentifiers :: [IdentifyCodec]
    , readExtents :: forall m . (Functor m, MonadIO m) => TrackNo -> Iteratee [Offset Block] m LayerExtents
    , rawConvEnee :: forall m . (Functor m, Monad m) => Enumeratee [Offset Block] [(TimeStamp, [a])] m ()
    , summaryConvEnee :: forall m . (Functor m, Monad m) => Enumeratee [Offset Block] [[Summary a]] m ()
    }

data ScopeRead = forall a . (ScopePlot a) => ScopeRead (ReadMethods a)

----------------------------------------------------------------------

data Scope ui = Scope
    { view   :: View ui
    , bounds :: Maybe (Range TimeStamp)
    , utcBounds :: Maybe (Range UTCTime)
    , layers :: [ScopeLayer]
    }

data View ui = View
    { viewX1 :: DataX
    , viewY1 :: Double
    , viewX2 :: DataX
    , viewY2 :: Double
    , pointerX :: Maybe CanvasX
    , dragDX :: Maybe DataX -- DataX of pointer at drag down
    , viewUI :: ui
    }

scopeNew :: ui -> Scope ui
scopeNew ui = Scope {
      view = viewInit ui
    , bounds = Nothing
    , utcBounds = Nothing
    , layers = []
    }

scopeClose :: Scope ui -> IO (Scope ui)
scopeClose scope = do
    mapM_ closeFd . nub . map fd' . layers $ scope
    return scope{bounds=Nothing, utcBounds=Nothing, layers=[]}
    where
        fd' (ScopeLayer l) = fd . layerFile $ l

scopeModifyView :: (View ui -> View ui) -> Scope ui -> Scope ui
scopeModifyView f scope = scope{ view = f (view scope) }

scopeTransform :: Transform DataX -> Scope ui -> Scope ui
scopeTransform tf = scopeModifyView (viewTransform tf)

viewInit :: ui -> View ui
viewInit = View (DataX 0.0) (-1.0) (DataX 1.0) 1.0 Nothing Nothing

viewTransform :: Transform DataX -> View ui -> View ui
viewTransform tf v@View{..} = v {
      viewX1 = transform tf viewX1
    , viewX2 = transform tf viewX2
    , dragDX = transform tf <$> dragDX
    }

scopeUpdate :: Maybe (Range TimeStamp)
            -> Maybe (Range UTCTime)
            -> Scope ui -> Scope ui
scopeUpdate newBounds Nothing scope =
    (t scope) { bounds = mb , utcBounds = Nothing }
    where
        oldBounds = bounds scope
        mb = unionBounds oldBounds newBounds
        t = case oldBounds of
                Just ob -> if oldBounds == mb
                               then id
                               else scopeTransform (mkTSDataTransform ob (fromJust mb))
                _ -> id

scopeUpdate newBounds (Just newUTCBounds) scope
    | (not . null . layers $ scope) && isNothing oldUTCBounds = scopeUpdate newBounds Nothing scope
    | otherwise = (t scope) { bounds = mb , utcBounds = umb }
    where
        oldBounds = bounds scope
        oldUTCBounds = utcBounds scope
        mb = unionBounds oldBounds newBounds
        umb = unionBounds oldUTCBounds (Just newUTCBounds)
        t = case oldUTCBounds of
                Just uob -> if oldUTCBounds == umb
                               then id
                               else scopeTransform (mkUTCDataTransform uob (fromJust umb))
                _ -> id

----------------------------------------------------------------------
