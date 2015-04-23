{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- | Parallel stream composition for Zeldspar.
module Parallel where
import Control.Monad
import Control.Monad.IO.Class ()
import Language.Embedded.Imperative
import Language.Embedded.Concurrent
import Ziria

import Language.C.Monad

import qualified Control.Concurrent as CC

import Data.Typeable

-- TODO: allow returning values? use explicit `forever` combinator?
data ParProg exp i o a where
  Lift     :: Z exp i o ()
           -> ParProg exp i o ()

  (:|>>>|) :: (VarPred exp i, VarPred exp o, VarPred exp t)
           => ParProg exp i t ()
           -> ParProg exp t o ()
           -> ParProg exp i o ()

-- | TODO: we want this instead when @newtype Z@ hits:
--
--    class Parallel p where
--      liftP :: p exp i o a -> ParProg exp i o a
--
--  ...because that's so much less messy.
class Parallel l r where
  type Par l r :: * -> *
  (|>>>|) :: l () -> r () -> Par l r ()

instance (VarPred exp i, VarPred exp o, VarPred exp t) =>
         Parallel (ParProg exp i t) (ParProg exp t o) where
  type Par (ParProg exp i t) (ParProg exp t o) = ParProg exp i o
  (|>>>|) = (:|>>>|)

instance (VarPred exp i, VarPred exp o, VarPred exp t) =>
         Parallel (Z exp i t) (ParProg exp t o) where
  type Par (Z exp i t) (ParProg exp t o) = ParProg exp i o
  a |>>>| b = Lift a :|>>>| b

instance (VarPred exp i, VarPred exp o, VarPred exp t) =>
         Parallel (ParProg exp i t) (Z exp t o) where
  type Par (ParProg exp i t) (Z exp t o) = ParProg exp i o
  a |>>>| b = a :|>>>| Lift b

instance (VarPred exp i, VarPred exp o, VarPred exp t) =>
         Parallel (Z exp i t) (Z exp t o) where
  type Par (Z exp i t) (Z exp t o) = ParProg exp i o
  a |>>>| b = (Lift a :|>>>| Lift b)

-- | Compile a parallel computation. Program terminates when the source throws
--   an exception or otherwise kills itself.
--   TODO: Jesus Christ, that type signature! ಠ_ಠ
compilePar
    :: ( EvalExp (IExp instr)
       , CompExp (IExp instr)
       , VarPred (IExp instr) Bool
       , VarPred (IExp instr) Int
       , VarPred (IExp instr) inp
       , VarPred (IExp instr) out
       , Typeable :< VarPred (IExp instr)
       , RefCMD (IExp instr)     :<: instr
       , ControlCMD (IExp instr) :<: instr
       , ThreadCMD               :<: instr
       , ChanCMD (IExp instr)    :<: instr
       )
    => ParProg (IExp instr) inp out ()
    -> Program instr (IExp instr inp)        -- ^ Source
    -> (IExp instr out -> Program instr ())  -- ^ Sink
    -> Program instr ()
compilePar ps inp out = do
  i <- newChan (litExp 10)
  o <- foldPP i ps $ \i p -> do
    o <- newChan (litExp 10)
    fork . void $ compile p (readChan i) (writeChan o)
    return o
  fork $ do
    while (return $ litExp True) $ readChan o >>= out
  while (return $ litExp True) $ inp >>= writeChan i

-- | Run a parallel Zeldspar computation.
runPar :: (EvalExp exp, Typeable :< VarPred exp, VarPred exp i, VarPred exp o)
       => ParProg exp i o ()
       -> IO i
       -> (o -> IO ())
       -> IO ()
runPar ps inp out = do
  i <- CC.newChan
  o <- foldPP i ps $ \i p -> do
    o <- CC.newChan
    CC.forkIO . void $ runIO p (CC.readChan i) (CC.writeChan o)
    return o
  CC.forkIO $ do
    forever $ CC.readChan o >>= out
  forever $ inp >>= CC.writeChan i

-- | Compile a parallel Zeldspar program to a 'String'.
icompilePar
    :: forall instr exp a . ( EvalExp exp
       , CompExp exp
       , VarPred exp Bool
       , VarPred exp Float
       , VarPred exp Int
       , Typeable :< VarPred (IExp instr)
       , instr ~ (RefCMD exp :+:
                  ControlCMD exp :+:
                  FileCMD exp :+:
                  ThreadCMD :+:
                  ChanCMD (IExp instr))
       )
    => ParProg exp Float Float () -> String
icompilePar prog =
    show $ prettyCGen $ liftSharedLocals $ wrapMain $ interpret cprog
  where
    cprog = do
        inp <- fopen "stdin" ReadMode
        out <- fopen "stdout" WriteMode
        compilePar prog (fget inp) (fput out) :: Program instr ()

-- | Left fold over a 'ParProg'.
foldPP :: (Monad m,
           Typeable :< VarPred exp,
           VarPred exp i,
           VarPred exp o)
       => chan i
       -> ParProg exp i o a
       -> (forall i o a. (VarPred exp i, VarPred exp o)
           => chan i
           -> Z exp i o a
           -> m (chan o))
       -> m (chan o)
foldPP acc (Lift p) f =
  f acc p
foldPP acc (a :|>>>| b) f =
  foldPP acc a f >>= \acc' -> foldPP acc' b f
