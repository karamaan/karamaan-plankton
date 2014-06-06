{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Karamaan.Plankton.Arrow.Orphans where

import qualified Control.Lens.Tuple as T
import Control.Lens.Indexed (indexed)
import Control.Lens ((<&>))

-- Oh no!  Orphans!
-- TODO: I don't know how indexed works.  Is it possible these are
-- written unsafely?
instance T.Field1 (a,b,c,d,e,f,g,h,i,a10,a11,a12)
                  (a',b,c,d,e,f,g,h,i,a10,a11,a12)
                   a a' where
  _1 k ~(a,b,c,d,e,f,g,h,i,a10,a11,a12) =
    indexed k (0 :: Int) a <&> \a' -> (a',b,c,d,e,f,g,h,i,a10,a11,a12)

instance T.Field1 (a,b,c,d,e,f,g,h,i,a10,a11,a12,a13,a14)
                  (a',b,c,d,e,f,g,h,i,a10,a11,a12,a13,a14)
                   a a' where
  _1 k ~(a,b,c,d,e,f,g,h,i,a10,a11,a12,a13,a14) =
    indexed k (0 :: Int) a <&> \a' -> (a',b,c,d,e,f,g,h,i,a10,a11,a12,a13,a14)

instance T.Field2 (a,b,c,d,e,f,g,h,i,a10,a11,a12,a13,a14)
                  (a,b',c,d,e,f,g,h,i,a10,a11,a12,a13,a14)
                   b b' where
  _2 k ~(a,b,c,d,e,f,g,h,i,a10,a11,a12,a13,a14) =
    indexed k (1 :: Int) b <&> \b' -> (a,b',c,d,e,f,g,h,i,a10,a11,a12,a13,a14)

instance T.Field3 (a,b,c,d,e,f,g,h,i,a10,a11,a12)
                  (a,b,c',d,e,f,g,h,i,a10,a11,a12)
                   c c' where
  _3 k ~(a,b,c,d,e,f,g,h,i,a10,a11,a12) =
    indexed k (2 :: Int) c <&> \c' -> (a,b,c',d,e,f,g,h,i,a10,a11,a12)

instance T.Field3 (a,b,c,d,e,f,g,h,i,a10,a11,a12,a13,a14)
                  (a,b,c',d,e,f,g,h,i,a10,a11,a12,a13,a14)
                   c c' where
  _3 k ~(a,b,c,d,e,f,g,h,i,a10,a11,a12,a13,a14) =
    indexed k (2 :: Int) c <&> \c' -> (a,b,c',d,e,f,g,h,i,a10,a11,a12,a13,a14)

instance T.Field4 (a,b,c,d,e,f,g,h,i,a10,a11,a12,a13,a14)
                  (a,b,c,d',e,f,g,h,i,a10,a11,a12,a13,a14)
                   d d' where
  _4 k ~(a,b,c,d,e,f,g,h,i,a10,a11,a12,a13,a14) =
    indexed k (3 :: Int) d <&> \d' -> (a,b,c,d',e,f,g,h,i,a10,a11,a12,a13,a14)

instance T.Field5 (a,b,c,d,e,f,g,h,i,a10,a11,a12)
                  (a,b,c,d,e',f,g,h,i,a10,a11,a12)
                   e e' where
  _5 k ~(a,b,c,d,e,f,g,h,i,a10,a11,a12) =
    indexed k (4 :: Int) e <&> \e' -> (a,b,c,d,e',f,g,h,i,a10,a11,a12)

instance T.Field6 (a,b,c,d,e,f,g,h,i,a10,a11,a12)
                  (a,b,c,d,e,f',g,h,i,a10,a11,a12)
                   f f' where
  _6 k ~(a,b,c,d,e,f,g,h,i,a10,a11,a12) =
    indexed k (5 :: Int) f <&> \f' -> (a,b,c,d,e,f',g,h,i,a10,a11,a12)

instance T.Field8 (a,b,c,d,e,f,g,h,i,a10,a11,a12)
                  (a,b,c,d,e,f,g,h',i,a10,a11,a12)
                  h h' where
  _8 k ~(a,b,c,d,e,f,g,h,i,a10,a11,a12) =
    indexed k (7 :: Int) h <&> \h' -> (a,b,c,d,e,f,g,h',i,a10,a11,a12)

instance T.Field9 (a,b,c,d,e,f,g,h,i,a10,a11,a12)
                  (a,b,c,d,e,f,g,h,i',a10,a11,a12)
                   i i' where
  _9 k ~(a,b,c,d,e,f,g,h,i,a10,a11,a12) =
    indexed k (8 :: Int) i <&> \i' -> (a,b,c,d,e,f,g,h,i',a10,a11,a12)
