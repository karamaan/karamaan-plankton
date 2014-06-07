module Karamaan.Plankton.Arrow.Lens where

import Prelude hiding (id)
import Control.Arrow (Arrow, arr, (<<<), second)
import Control.Category (id)
import qualified Lens.Family as L

viewA :: Arrow arr => L.FoldLike a s t a b -> arr s a
viewA = arr . L.view

overA :: Arrow p => L.LensLike (Context a b) s t a b -> p a b -> p s t
overA l p = arr (uncurry id) <<< second p <<< arr go where
    go s = case l sell s of
      Context f a -> (f, a)

-- This is %%~ from Control.Lens but I prefer a name rather than symbols
-- It isn't to do with /Arrow/ lenses per se, but I put it here rather than
-- starting a new module just for one function.
-- NB: It's probably better just to use 'traverseOf'
overf :: Functor f => L.LensLike f s t a b -> (a -> f b) -> s -> f t
overf = id

-- Context stuff which is from lens.  Since we don't want to import
-- lens we replicate it here.  Arguably it could be in
-- lens-family-core.

data Context a b t = Context (b -> t) a

sell :: a -> Context a b b
sell = Context id

instance Functor (Context a b) where
  fmap f (Context g t) = Context (f . g) t
  {-# INLINE fmap #-}
