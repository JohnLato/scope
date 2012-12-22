{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
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
