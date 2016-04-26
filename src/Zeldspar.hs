-- | An implementation of Ziria that uses Feldspar to represent computations
module Zeldspar where

import Feldspar.Run
import Ziria


--------------------------------------------------------------------------------
-- * Representation and translation
--------------------------------------------------------------------------------

type Zun inp out = Z inp out Run

translate :: forall inp out a. Zun inp out ()
          -> (Run inp)        -- ^ Source
          -> (out -> Run ())  -- ^ Sink
          -> Run ()
translate (Z p) src snk = trans (p (\_ -> Stop))
  where
    trans :: Action inp out Run -> Run ()
    trans (Lift rp)     = rp >>= \a -> trans a
    trans (Emit x p)    = snk x >> trans p
    trans (Receive p)   = src >>= trans . p
    trans Stop          = return ()
    trans (Loop p)      = while (return true) (void $ trans p)
    trans (Times n p k) = for (0, 1, Excl $ value n) (const $ void $ trans p)
                       >> trans k


--------------------------------------------------------------------------------
-- * Utilities
--------------------------------------------------------------------------------

store :: Storable a => Zun a a ()
store = do
    i <- receive
    s <- lift $ initStore i
    o <- lift $ unsafeFreezeStore s
    emit o
