-- | Parallel stream composition for Ziria
module Ziria.Parallel where

import Ziria


--------------------------------------------------------------------------------
-- * Representation
--------------------------------------------------------------------------------

data ParZ inp out m a
  = LiftP (Z inp out m ())
  | forall mid. (ParZ inp mid m ()) :|>>>| (ParZ mid out m ())


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

(|>>>|) :: (Parallel a, Parallel b, Monad m)
        => a inp mid m () -> b mid out m () -> ParZ inp out m ()
l |>>>| r = liftP l :|>>>| liftP r


-- | Left fold over a 'ParZ'
foldParZ :: Monad m
         => c inp
         -> ParZ inp out m a
         -> (forall inp out a. c inp -> Z inp out m a -> m (c out))
         -> m (c out)
foldParZ acc (LiftP p)    f = f acc p
foldParZ acc (a :|>>>| b) f = foldParZ acc a f >>= \acc' -> foldParZ acc' b f

