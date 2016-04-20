-- | A generic implementation of sequential Ziria programs
module Ziria where

import Control.Monad
import qualified Control.Monad.Trans.Class as MT
import Feldspar.Run


--------------------------------------------------------------------------------
-- * Representation
--------------------------------------------------------------------------------

data Action m inp out
  = Lift (m (Action m inp out))
  | Emit out (Action m inp out)
  | Receive (inp -> Action m inp out)
  | Stop
  | Loop (Action m inp out)
  | Times Length (Action m inp out) (Action m inp out)

newtype Z inp out m a = Z ((a -> Action m inp out) -> Action m inp out)

instance Functor (Z inp out m) where
  fmap f (Z z) = Z (\k -> z (k . f))

instance Monad m => Applicative (Z inp out m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (Z inp out m) where
  return x  = Z (\k -> k x)
  Z z >>= h = Z (\k -> z (\x -> let Z z' = h x in z' k))

instance MT.MonadTrans (Z inp out) where lift = lift


--------------------------------------------------------------------------------
-- * Front end
--------------------------------------------------------------------------------

lift :: Monad m => m a -> Z inp out m a
lift r = Z (\k -> Lift (r >>= (return . k)))

emit :: out -> Z inp out m ()
emit x = Z (\k -> Emit x (k ()))

receive :: Z inp out m inp
receive = Z (\k -> Receive k)

loop :: Z inp out m () -> Z inp out m ()
loop (Z z) = Z (\_ -> Loop (z (\_ -> Stop)))

times :: Length -> Z inp out m () -> Z inp out m ()
times n (Z z) = Z (\k -> Times n (z (\_ -> Stop)) (k ()))


--------------------------------------------------------------------------------
-- * Pipelining
--------------------------------------------------------------------------------

(>>>) :: Monad m => Z inp mid m () -> Z mid out m () -> Z inp out m ()
Z p >>> Z q = Z (\k -> fuse (p (\_ -> Stop)) (q (\_ -> Stop)) (k ()))

fuse :: Monad m => Action m inp mid -> Action m mid out -> Action m inp out -> Action m inp out
fuse (Emit x p)  (Receive q) k = fuse p (q x) k
fuse (Lift rp)   q           k = Lift ((\p -> fuse p q k) `fmap` rp)
fuse p           (Lift rq)   k = Lift ((\q -> fuse p q k) `fmap` rq)
fuse _           Stop        k = k
fuse p           (Emit x q)  k = Emit x (fuse p q k)
fuse (Receive p) q           k = Receive (\x -> fuse (p x) q k)
fuse Stop        _           k = k

