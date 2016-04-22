-- | An implementation of Ziria that uses Feldspar to represent computations
module Zeldspar where

import Feldspar.Run
import Ziria


--------------------------------------------------------------------------------
-- * Representation and translation
--------------------------------------------------------------------------------

type Zun inp out = Z inp out Run

translate :: forall inp out a .
             Zun inp out a
          -> (Run inp)        -- ^ Source
          -> (out -> Run ())  -- ^ Sink
          -> Run a
translate (Z p) src snk = trans (p Return)
  where
    trans :: forall a . Action inp out Run a -> Run a
    trans (Lift m p)    = m >>= \a -> trans (p a)
    trans (Emit x p)    = snk x >> trans p
    trans (Receive p)   = src >>= trans . p
    trans (Return x)    = return x
    trans (Loop s0 p)   = do st <- initStore s0
                             while (return true) $
                               do s <- readStore st
                                  s' <- trans (p s)
                                  writeStore st s'
                             return (error "does not terminate")

--------------------------------------------------------------------------------
-- * Utilities
--------------------------------------------------------------------------------

store :: Storable a => Zun a a ()
store = loop $ do
    i <- receive
    s <- lift $ initStore i
    o <- lift $ unsafeFreezeStore s
    emit o
