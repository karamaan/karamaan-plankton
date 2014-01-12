module Karamaan.Plankton.Arrow.ReaderCurry where

import Control.Arrow ((&&&), Arrow)
import Control.Category ((<<<))

-- The transformation provided here is a bit like currying followed by
-- lifting into the Reader monad.  It's not *exactly* that, but I don't
-- have a better idea for a name currently.

readerCurry2 :: Arrow arr => arr (a, b) r -> arr z a -> arr z b -> arr z r
readerCurry2 op f g = op <<< (f &&& g)

readerCurry1 :: Arrow arr => arr a r -> arr z a -> arr z r
readerCurry1 = (<<<)
