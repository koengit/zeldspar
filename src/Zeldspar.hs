-- | An implementation of Ziria that uses Feldspar to represent computations
module Zeldspar where

import Feldspar.Run
import Ziria


type Zun inp out = Z inp out Run

translate :: forall inp out a. Zun inp out a
          -> (Run inp)        -- ^ Source
          -> (out -> Run ())  -- ^ Sink
          -> Run a
translate (Z p) src snk = trans (p (\_ -> Stop))
  where
    trans :: Action inp out Run -> Run a
    trans (Lift rp)     = rp >>= \a -> trans a
    trans (Emit x p)    = snk x >> trans p
    trans (Receive p)   = src >>= trans . p
    trans Stop          = end
    trans (Loop p)      = while (return true) (void $ trans p) >> end
    trans (Times n p k) = for (0, 1, Excl $ value n) (const $ void $ trans p) >> trans k
    
    end :: Run a
    end = return $ error "unreachable"

