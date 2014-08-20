module Karamaan.Plankton where

import Data.List (intercalate)

putLines :: Show a => [a] -> IO ()
putLines = putStrLn . intercalate "\n" . map show

uncurry3 :: (t1 -> t2 -> t3 -> t) -> (t1, t2, t3) -> t
uncurry3 f (x, y, z) = f x y z

infix 8 .:

(.:) :: (i -> r) -> (a1 -> a2 -> i) -> (a1 -> a2 -> r)
(f .: g) x y = f (g x y)

(.:.) :: (r -> z) -> (a -> b -> c -> r) -> (a -> b -> c -> z)
(f .:. g) x y z = f (g x y z)

(.:::.) :: (d -> e) -> (a -> a1 -> a2 -> a3 -> a4 -> b -> c -> d)
        -> a -> a1 -> a2 -> a3 -> a4 -> b -> c -> e
(f .:::. g) t u v w x y z = f (g t u v w x y z)

zipWithLongest :: a -> b -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithLongest xdef ydef f = zip' where
  zip' [] [] = []
  zip' (x:xs) [] = zip' (x:xs) [ydef]
  zip' [] (y:ys) = zip' [xdef] (y:ys)
  zip' (x:xs) (y:ys) = f x y : zip' xs ys
