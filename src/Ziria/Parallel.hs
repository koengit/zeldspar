-- | Parallel stream composition for Ziria
module Ziria.Parallel where

import Feldspar.Run.Concurrent
import Ziria


--------------------------------------------------------------------------------
-- * Representation
--------------------------------------------------------------------------------

data ParZ inp out m a
  = LiftP (Z inp out m ())
  | forall mid. (Transferable mid)
    => ConnP (SizeSpec mid) (ParZ inp mid m ()) (ParZ mid out m ())


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

-- | Parallel composition of Ziria programs. It should be used in conjunction
--   with (>>|) to create a mixfix operator with a channel size specifier in the
--   middle, for example 'a |>>n>>| b' composes 'a' and 'b' through a channel of
--   size 'n'.
(|>>) :: (Parallel a, Monad m, Transferable mid)
      => a inp mid m ()
      -> SizeSpec mid
      -> (ParZ mid out m () -> ParZ inp out m ())
l |>> len = ConnP len (liftP l)

(>>|) :: (Parallel a, Monad m, Transferable mid)
      => (ParZ mid out m () -> ParZ inp out m ())
      -> a mid out m ()
      -> ParZ inp out m ()
connP >>| r = connP (liftP r)

infixl 1 |>>
infixl 1 >>|
