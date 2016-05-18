-- | A generic implementation of sequential Ziria programs
module Ziria where

import Feldspar


--------------------------------------------------------------------------------
-- * Representation of Actions
--------------------------------------------------------------------------------

data Action inp out m a where
  Lift    :: m b -> (b -> Action inp out m a) -> Action inp out m a
  Emit    :: out -> Action inp out m a -> Action inp out m a
  Receive :: (inp -> Action inp out m a) -> Action inp out m a
  Return  :: a -> Action inp out m a
  Loop    :: Storable a => a -> (a -> Action inp out m a) -> Action inp out m b

instance Monad m => Functor (Action inp out m) where
  fmap f m = m >>= \x -> return (f x)

instance Monad m => Applicative (Action inp out m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (Action inp out m) where
  return x = Return x

  Lift m p  >>= k = Lift m (\x -> p x >>= k)
  Emit x p  >>= k = Emit x (p >>= k)
  Receive p >>= k = Receive (\x -> p x >>= k)
  Return x  >>= k = k x
  Loop x p  >>= k = Loop x p


--------------------------------------------------------------------------------
-- * Z monad
--------------------------------------------------------------------------------

newtype Z inp out m a
  = Z (forall b . (a -> Action inp out m b) -> Action inp out m b)

instance Functor (Z inp out m) where
  fmap f (Z z) = Z (\k -> z (k . f))

instance Monad m => Applicative (Z inp out m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (Z inp out m) where
  return x  = Z (\k -> k x)
  Z z >>= h = Z (\k -> z (\x -> let Z z' = h x in z' k))


--------------------------------------------------------------------------------
-- * Front end
--------------------------------------------------------------------------------

lift :: Monad m => m a -> Z inp out m a
lift m = Z (\k -> Lift m k)

emit :: out -> Z inp out m ()
emit x = Z (\k -> Emit x (k ()))

receive :: Z inp out m inp
receive = Z (\k -> Receive k)

loop :: Z inp out m a -> Z inp out m b
loop (Z z) = Z (\_ -> Loop () (\_ -> z (\_ -> Return ())))


--------------------------------------------------------------------------------
-- * Lifting input and output
--------------------------------------------------------------------------------

liftIn :: (inp' -> m inp) -> Z inp out m a -> Z inp' out m a
liftIn f (Z p) = Z (\k -> (liftIn' f (p Return) k))

liftIn' :: (inp' -> m inp)
        -> Action inp out m a
        -> (a -> Action inp' out m b)
        -> Action inp' out m b
liftIn' f (Lift m p)  k = Lift m (\x -> liftIn' f (p x) k)
liftIn' f (Emit x p)  k = Emit x (liftIn' f p k)
liftIn' f (Receive p) k = Receive (\x -> Lift (f x) (\x' -> liftIn' f (p x') k))
liftIn' f (Return x)  k = k x
liftIn' f (Loop x p)  k = Loop x (\x -> liftIn' f (p x) Return)


liftOut :: (out -> m out') -> Z inp out m a -> Z inp out' m a
liftOut f (Z p) = Z (\k -> (liftOut' f (p Return) k))

liftOut' :: (out -> m out')
         -> Action inp out m a
         -> (a -> Action inp out' m b)
         -> Action inp out' m b
liftOut' f (Lift m p)  k = Lift m (\x -> liftOut' f (p x) k)
liftOut' f (Emit x p)  k = Lift (f x) (\x' -> Emit x' (liftOut' f p k))
liftOut' f (Return x)  k = k x
liftOut' f (Receive p) k = Receive (\x -> liftOut' f (p x) k)
liftOut' f (Loop x p)  k = Loop x (\x -> liftOut' f (p x) Return)


--------------------------------------------------------------------------------
-- * Pipelining
--------------------------------------------------------------------------------

(>>>) :: (Monad m, Storable mid) => Z inp mid m () -> Z mid out m () -> Z inp out m ()
Z p >>> Z q = Z (\k -> fuse (p Return) (q Return) (\_ _ -> k ()) (\_ _ -> k ()))

fuse :: (Monad m, Storable mid)
     => Action inp mid m a -> Action mid out m b
     -> (a -> Action mid out m b -> Action inp out m c)
     -> (Action inp mid m a -> b -> Action inp out m c)
     -> Action inp out m c
fuse (Emit x p)  q           k1 k2
  | hasReceive q                   = fuse p (push x q) k1 k2
fuse (Lift m p)  q           k1 k2 = Lift m (\x -> fuse (p x) q k1 k2)
fuse p           (Lift m q)  k1 k2 = Lift m (\y -> fuse p (q y) k1 k2)
fuse p           (Return y)  k1 k2 = k2 p y
fuse p           (Emit x q)  k1 k2 = Emit x (fuse p q k1 k2)
fuse (Receive p) q           k1 k2 = Receive (\x -> fuse (p x) q k1 k2)
fuse (Return x)  q           k1 k2 = k1 x q
fuse (Emit x p)  q           k1 k2 = fuse (Loop () Return) q k1 k2

fuse (Loop x p)  (Loop y q)  _  _  = fuseLoops x p y q
fuse (Loop x p)  q           k1 k2 = fuse (p x >>= \x' -> Loop x' p) q k1 k2

hasReceive :: Action inp out m a -> Bool
hasReceive (Emit _ p)  = hasReceive p
hasReceive (Receive _) = True
hasReceive (Lift _ p)  = hasReceive (p (error "don't look at the result of lift"))
hasReceive (Return _)  = False
hasReceive (Loop x p)  = hasReceive (p x)

push :: (Monad m, Storable inp) => inp -> Action inp out m a -> Action inp out m a
push x (Emit y p)  = Emit y (push x p)
push x (Receive p) = p x
push x (Lift m p)  = Lift m (\y -> push x (p y))
push x (Return a)  = error "pushing a message into a return"
push x (Loop s p)  =
  Loop (s,x) (\(s,x) ->
    push x (p s) >>= \s' ->
    Receive $ \x' ->
    Return (s',x')
  )

fuseLoops :: (Monad m, Storable mid, Storable a, Storable b)
          => a -> (a -> Action inp mid m a)
          -> b -> (b -> Action mid out m b)
          -> Action inp out m c
fuseLoops x p y q =
  Loop (x,y) (\(x,y) ->
    fuse (p x) (q y) k1 k2
  )
 where
  k1 x          q' = fuse (p x) q' k1 k2
  k2 (Return x) y  = Return (x,y)
  k2 p'         y  = fuse p' (q y) k1 k2
