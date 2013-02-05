{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------

{- |
--   Module      : Scope.Sources.ZoomCache
--   Copyright   : Conrad Parker, John Lato
--   License     : BSD3-style (see LICENSE)
--
--   Maintainer  : Conrad Parker
--   <conrad@metadecks.org>
--   Stability   : unstable
--   Portability : unknown
--
--   Source instances for zoom-cache files
-}
----------------------------------------------------------------------

-- | Generate a data @Source@ from a 'ZoomCache' file
module Scope.Sources.ZoomCache
(
  ScopeFile (..)
, genScopeFile
, scopeFileSource
, addLayersFromFile
) where

import           Scope.Types
import           Scope.Numeric.IEEE754

import qualified Data.IntMap as IM
import           Data.Iteratee as I
import qualified Data.Iteratee.IO.OffsetFd as OffI
import qualified Data.List as L
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic.Base as B
import qualified Data.Vector.Generic.Mutable as B
import           Data.ZoomCache.Numeric
import           Data.Offset

import           Data.ByteString (ByteString)
import           Control.Applicative
import           Control.Monad.CatchIO

scopeBufSize :: Int
scopeBufSize = 1024

addLayersFromFile :: Plot TimeStamp Double diagram
                  -> FilePath
                  -> Scope diagram ui
                  -> IO (Scope diagram ui)
addLayersFromFile plot filepath scope = do
    source <- scopeFileSource filepath
    addPlot source plot Sample scope

scopeFileSource :: FilePath -> IO (Source TimeStamp Double)
scopeFileSource filepath = do
    sf <- genScopeFile standardIdentifiers filepath
    -- TODO: rescan extents if the file has been modified
    extents <- scopeExtents sf
    return $ Source
        { sourceExtent = return extents
        , genSourceProvider = error "ScopeFile source uninitialized"
        }

-- probably shouldn't be double, should be min/max/mean or something
-- also, multi-track files should probably return a tuple or something.
mkScopeSourceProvider :: (y ~ Double)
                      => ScopeFile TimeStamp y
                      -> Scaling (TimeStamp, y)
                      -> IO (Hint
                             -> Range TimeStamp
                             -> IO (U.Vector (TimeStamp, y)))
mkScopeSourceProvider sf@ScopeFile{sfCache} _scaling =
  return $ \hint range -> do
    let nSz = unHint hint
        (minX,maxX) = toBounds range
        doInBounds iter = do
              seekTimeStamp sfCache (Just minX)
              joinI $ I.breakE (before (Just maxX)) iter
        someIter = fmap U.fromList $ joinI $ enumSummaryDouble 1
                         ><> mapStream (\p -> ( fromJust (timestamp p)
                                              , numAvg $ summaryData p))
                         $ stream2stream
    scopeEnum sf $ enumBlock sfCache =$ doInBounds someIter

deriving instance U.Unbox TimeStamp
deriving instance (B.Vector U.Vector TimeStamp)
deriving instance (B.MVector U.MVector TimeStamp)

fromJust = maybe (error "mkScopeSourceProvider: no timestamp") id

scopeExtents :: ScopeFile TimeStamp Double -> IO (Range TimeStamp, Range Double)
scopeExtents sf@ScopeFile{sfCache} = do
    let tracks = IM.keys $ cfSpecs sfCache
    extents <- mapM (scopeEnum sf . I.joinI . enumBlock sfCache . extentsDouble)
                    tracks
    return $ L.foldl1' (\(l1,r1) (l2,r2) -> (unionRange l1 l2, unionRange r1 r2)) extents

data ScopeFile extents dtype = ScopeFile
    { sfPath  :: FilePath
    , sfCache :: CacheFile
    }

-- | Create a new ScopeFile.
genScopeFile :: [IdentifyCodec] -> FilePath -> IO (ScopeFile TimeStamp Double)
genScopeFile readIdentifiers path = do
    let f = ScopeFile path (error "ScopeFile uninitialized")
    sfCache <- scopeEnum f (iterHeaders readIdentifiers)
    return f{sfCache}

scopeEnum :: MonadCatchIO m
          => ScopeFile extents dtype
          -> I.Iteratee (Offset ByteString) m a
          -> m a
scopeEnum ScopeFile{..} iter =
    OffI.enumFileRandomOBS scopeBufSize sfPath iter >>= I.run

instance AffineSpace TimeStamp where
    type Diff TimeStamp = Double
    TS l .-. TS r = l .-. r
    TS t .+^ diff = TS $ t .+^ diff
