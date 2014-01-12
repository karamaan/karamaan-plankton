{-# LANGUAGE Arrows #-}

module Karamaan.Plankton.Arrow where

import Prelude hiding (id)
import Control.Arrow ((&&&), Arrow, arr, returnA, second)
import Control.Category ((<<<), id)
import Data.List (foldl')

voidArr :: Arrow arr => arr a ()
voidArr = arr (const ())

andVoid :: Arrow arr => arr a () -> arr a () -> arr a ()
andVoid f g = voidArr <<< (f &&& g)

all_ :: Arrow arr => [arr a ()] -> arr a ()
all_ = foldl' andVoid voidArr

replaceWith :: Arrow arr => arr () b -> arr a b
replaceWith = (<<< voidArr)

restrictWith :: Arrow arr => arr b () -> arr b b
restrictWith r = arr fst <<< (id &&& r)

removeUnit :: Arrow arr => arr a (a, ())
removeUnit = arr (flip (,) ())

liftArr2 :: Arrow arr => (a -> b -> r) -> arr z a -> arr z b -> arr z r
liftArr2 f x y = f <$-> x <*-> y

-- TODO: I guess this should really be made to work with any traversal
-- cf 'instance Traversable []' in Data.Traversable
sequenceArr :: Arrow arr => [arr a b] -> arr a [b]
sequenceArr = foldr (liftArr2 (:)) (arr (const []))

traverseArr :: Arrow arr => (b -> arr a c) -> [b] -> arr a [c]
traverseArr f = sequenceArr . map f

(<*->) :: Arrow arr => arr a (b -> c) -> arr a b -> arr a c
f <*-> x = proc a -> do
  f' <- f -< a
  x' <- x -< a
  returnA -< f' x'

(<$->) :: Arrow arr => (b -> c) -> arr a b -> arr a c
f <$-> x = arr f <<< x

foldrArr :: Arrow arr => arr (a, b) b -> arr z b -> [arr z a] -> arr z b
foldrArr f = foldr g
  where g x rest = f <<< (x &&& rest)

foldl'Arr :: Arrow arr => arr (b, a) b -> arr z b -> [arr z a] -> arr z b
foldl'Arr f = foldl' g
  where g rest x = f <<< (rest &&& x)

-- opC stands for "operator curry".  It's a sort of partial application.
-- If anyone has a better name, please change it :)
opC :: Arrow arr => arr (a, b) c -> arr () b -> arr a c
opC op q = op <<< second q <<< arr (\a -> (a, ()))

noOp :: Arrow arr => arr a ()
noOp = arr (const ())
