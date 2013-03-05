{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -Wall #-}
----------------------------------------------------------------------
{- |
   Module      : Scope.Layer
   Copyright   : Conrad Parker
   License     : BSD3-style (see LICENSE)

   Maintainer  : Conrad Parker <conrad@metadecks.org>
   Stability   : unstable
   Portability : unknown

   Layers

-}
----------------------------------------------------------------------

module Scope.Layer (
  scopeRender
, scopeRender'
, setCache
) where

import Control.Monad

import Diagrams.Prelude hiding (width, view)

import Scope.Types

----------------------------------------------------------------

-- | Generate the diagram from a Scope, using the provided width value.
--
-- The cache will be used if it's current.
scopeRender :: Monoid' m => Int -> Scope (QDiagram b R2 m) ui -> IO (QDiagram b R2 m)
scopeRender = scopeRender' True

scopeRender' :: Monoid' m => Bool -> Int -> Scope (QDiagram b R2 m) ui -> IO (QDiagram b R2 m)
scopeRender' useCache width Scope{..} = do
    let xRng = viewX view
        yRng = viewY view
        hint = Hint width
    case useCache of
        True | Just (xCache, yCache, diagCache) <- scopeCache
          , xCache == xRng
          , yCache == yRng -> return diagCache
        otherwise -> foldM (\diag layer -> (`atop` diag) <$> layerRender layer hint xRng yRng) mempty layers

setCache :: Monoid' m
         => QDiagram b R2 m
         -> Scope (QDiagram b R2 m) ui
         -> Scope (QDiagram b R2 m) ui
setCache diag scope@Scope{view} = scope{scopeCache}
  where
    View{viewX,viewY} = view
    scopeCache = Just (viewX,viewY,diag)

-- | render a layer to a diagram with size in the range 0-1 (square), with the
-- local origin centered in the middle.
layerRender :: Monoid' m
            => Layer (QDiagram b R2 m)
            -> Hint
            -> Range DataX
            -> Range DataY
            -> IO (QDiagram b R2 m)
layerRender Layer{..} hint xRng yRng = do
    diag <- plotRenderer layerRenderer hint xRng yRng
    return $ diag # scaleToX 1 # scaleToY 1 # centerXY
