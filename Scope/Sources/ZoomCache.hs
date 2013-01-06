{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
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
) where

import           Scope.Types
import           Scope.Numeric.IEEE754

import qualified Data.IntMap as IM
import           Data.Iteratee as I
import qualified Data.Iteratee.IO.OffsetFd as OffI
import qualified Data.List as L
import           Data.ZoomCache.Numeric
import           Data.Offset

import           Data.ByteString (ByteString)
import           Control.Monad.CatchIO

scopeBufSize :: Int
scopeBufSize = 1024

{-
data Source sourceX sourceY = Source
   { sourceExtent      :: IO (Range sourceX, Range sourceY)
   , genSourceProvider :: Scaling (sourceX,sourceY)
                          -> IO (Hint
                                -> Range (sourceX)
                                -> IO (U.Vector (sourceX, sourceY)))
   }
-}

scopeFileSource :: FilePath -> IO (Source TimeStamp Double)
scopeFileSource filepath = do
    sf <- genScopeFile standardIdentifiers filepath
    -- TODO: rescan extents if the file has been modified
    extents <- scopeExtents sf
    return $ Source
        { sourceExtent = return extents
        }

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
    let f = ScopeFile path undefined
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
