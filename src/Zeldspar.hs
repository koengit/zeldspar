-- | An implementation of Ziria that uses Feldspar to represent computations
module Zeldspar
  ( module F
  , module Z
  , Zun
  , translate
  , precompute
  , store
  ) where

import Feldspar as F hiding (foldM)
import Feldspar.Data.Vector as F
import Feldspar.Run as F hiding (foldM)
import Ziria as Z


--------------------------------------------------------------------------------
-- * Representation and translation
--------------------------------------------------------------------------------

type Zun inp out = Z inp out Run

translate :: forall inp out m a. MonadComp m
          => Z inp out m a
          -> (m inp)        -- ^ Source
          -> (out -> m ())  -- ^ Sink
          -> m a
translate (Z p) src snk = trans (p Return)
  where
    trans :: forall a. Action inp out m a -> m a
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

precompute :: (MonadComp m, Storable a) => a -> m a
precompute x = do
  s <- initStore x
  unsafeFreezeStore s

store :: Storable a => Zun a a ()
store = do
  i <- receive
  o <- lift $ precompute i
  emit o
