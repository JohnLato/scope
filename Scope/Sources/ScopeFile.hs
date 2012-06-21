{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------

{- |
--   Module      : Scope.Sources.ScopeFile
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
module Scope.Sources.ScopeFile
(
  ScopeFile (..)
, genScopeFile
) where

import           Scope.Types

import           Data.Iteratee as I
import qualified Data.Iteratee.IO.OffsetFd as OffI
import Data.ZoomCache.Numeric
import Data.Offset

import Data.ByteString (ByteString)
import Control.Monad.CatchIO

scopeBufSize :: Int
scopeBufSize = 1024


data ScopeFile extents dtype = ScopeFile
    { filename :: FilePath
    , scopeCF  :: CacheFile
    }

{-
instance Source (ScopeFile (v a)) where
    type SourceData (VectorSource (v a)) = v a

    {-# INLINE genSourceProvider #-}
    genSourceProvider = vsourceProvider
    -}


-- | Create a new ScopeFile.
genScopeFile :: ReadMethods extents a -> FilePath -> IO (ScopeFile extents a)
genScopeFile ReadMethods{..} path = do
    let f = ScopeFile path undefined
    cf <- scopeEnum f (iterHeaders readIdentifiers)
    return f{scopeCF = cf}

scopeEnum :: MonadCatchIO m
          => ScopeFile extents dtype
          -> I.Iteratee (Offset ByteString) m a
          -> m a
scopeEnum ScopeFile{..} iter =
    OffI.enumFileRandomOBS scopeBufSize filename iter >>= I.run


{-
vectorSource :: V.Vector v a => v a -> VectorSource (v a)
vectorSource = VectorSource

instance (V.Vector v a) => Source (VectorSource (v a)) where
    type SourceData (VectorSource (v a)) = v a

    {-# INLINE genSourceProvider #-}
    genSourceProvider = vsourceProvider

-- | Use a vector as a data source.  @Scaling@ is ignored, and no 'IO' is
-- performed.
vsourceProvider
    :: (V.Vector v a)
    => VectorSource (v a)
    -> Scaling (v a)
    -> IO (Viewport -> IO (v a))
vsourceProvider (VectorSource vec) _ = return viewfunc
  where
    viewfunc = return . fromExtent . flip viewRange dataExtent . viewExtentX

    dataExtent :: Range DataX
    dataExtent = fromBoundsC 0 (fromIntegral (V.length vec) - 1)

    fromExtent (toBounds -> (DataX minD,DataX maxD)) =
        let minbound = min 0 (floor minD)
            maxrem   = V.length vec - minbound
        in  V.slice minbound (max maxrem (ceiling maxD)) vec
        -}
