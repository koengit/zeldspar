module Z where

import Control.Monad
import Feldspar.Run

---

data Action inp out
  = Lift (Run (Action inp out))
  | Emit out (Action inp out)
  | Receive (inp -> Action inp out)
  | Stop
  | Loop (Action inp out)
  | Times Int (Action inp out) (Action inp out)

instance Functor (Z inp out) where
  fmap f (Z z) = Z (\k -> z (k . f))
  
instance Applicative (Z inp out) where
  pure  = return
  (<*>) = ap
  
instance Monad (Z inp out) where
  return x  = Z (\k -> k x)
  Z z >>= h = Z (\k -> z (\x -> let Z z' = h x in z' k))

newtype Z inp out a =
  Z ((a -> Action inp out) -> Action inp out)

lift :: Run a -> Z inp out a
lift r = Z (\k -> Lift (r >>= (return . k)))

emit :: out -> Z inp out ()
emit x = Z (\k -> Emit x (k ()))

receive :: Z inp out inp
receive = Z (\k -> Receive k)

loop :: Z inp out () -> Z inp out ()
loop (Z z) = Z (\_ -> Loop (z (\_ -> Stop)))

times :: Int -> Z inp out () -> Z inp out ()
times n (Z z) = Z (\k -> Times n (z (\_ -> Stop)) (k ()))

---

(->>-) :: Z inp mid () -> Z mid out () -> Z inp out ()
Z p ->>- Z q = Z (\k -> fuse (p (\_ -> Stop)) (q (\_ -> Stop)) (k ()))

fuse :: Action inp mid -> Action mid out -> Action inp out -> Action inp out
fuse (Emit x p)  (Receive q) k = fuse p (q x) k
fuse (Lift rp)   q           k = Lift ((\p -> fuse p q k) `fmap` rp)
fuse p           (Lift rq)   k = Lift ((\q -> fuse p q k) `fmap` rq)
fuse _           Stop        k = k
fuse p           (Emit x q)  k = Emit x (fuse p q k)
fuse (Receive p) q           k = Receive (\x -> fuse (p x) q k)
fuse Stop        _           k = k



