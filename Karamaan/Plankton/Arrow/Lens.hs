{-# LANGUAGE FlexibleContexts #-}

module Karamaan.Plankton.Arrow.Lens where

import Prelude hiding (id)
import Control.Arrow (Arrow, arr, (<<<), second)
import Control.Category (id)
import Karamaan.Plankton.Arrow.Orphans ()
import qualified Control.Lens as L
import qualified Control.Lens.Internal.Context as LC

viewA :: Arrow arr => L.Getting a s a -> arr s a
viewA = arr . L.view

overA :: Arrow p => L.LensLike (L.Context a b) s t a b -> p a b -> p s t
overA l p = arr (uncurry id) <<< second p <<< arr go where
    go s = case l LC.sell s of
      L.Context f a -> (f, a)

-- This is %%~ from Control.Lens but I prefer a name rather than symbols
-- It isn't to do with /Arrow/ lenses per se, but I put it here rather than
-- starting a new module just for one function.
overf :: Functor f => ((a -> f b) -> s -> f t) -> (a -> f b) -> s -> f t
overf = id
