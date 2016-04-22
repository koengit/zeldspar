-- | Parallel stream composition for Ziria
module Ziria.Parallel where

import Feldspar.Run.Concurrent
import Ziria


--------------------------------------------------------------------------------
-- * Representation
--------------------------------------------------------------------------------

data ParZ inp out m a
  = LiftP (Z inp out m ())
  | forall mid. Transferable mid => (ParZ inp mid m ()) :|>>>| (ParZ mid out m ())


--------------------------------------------------------------------------------
-- * Front end
--------------------------------------------------------------------------------

class Parallel p where
  -- | Lift a computation to the parallel level.
  liftP :: p inp out m () -> ParZ inp out m ()

instance Parallel ParZ where
  liftP = id

instance Parallel Z where
  liftP = LiftP

(|>>>|) :: (Parallel a, Parallel b, Monad m, Transferable mid)
        => a inp mid m () -> b mid out m () -> ParZ inp out m ()
l |>>>| r = liftP l :|>>>| liftP r
