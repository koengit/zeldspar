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

-- | Interpret 'ParProg' in the 'IO' monad
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

-- | Translate 'ParProg' to 'Program'. The resulting program terminates when the source throws an
-- exception or otherwise kills itself.
translatePar
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
translatePar ps inp out = do
  i <- newChan (litExp 10)
  o <- foldPP i ps $ \i p -> do
    o <- newChan (litExp 10)
    fork . void $ translate p (readChan i) (writeChan o)
    return o
  fork $ do
    while (return $ litExp True) $ readChan o >>= out
  while (return $ litExp True) $ inp >>= writeChan i

-- | Simplified compilation from 'ParProg' to C. Input/output is done via two external functions:
-- @source@ and @sink@.
compileParStr :: forall exp inp out
    .  ( EvalExp exp
       , CompExp exp
       , VarPred exp Bool
       , VarPred exp Int
       , VarPred exp inp
       , VarPred exp out
       , Typeable :< VarPred exp
       )
    => ParProg exp inp out () -> String
compileParStr prog =
    show $ prettyCGen $ liftSharedLocals $ wrapMain $ interpret cprog
  where
    src   = callFun "source" []
    snk   = \o -> callProc "sink" [FunArg o]
    cprog = translatePar prog src snk
              :: Program ((RefCMD exp :+:
                           ControlCMD exp :+:
                           FileCMD exp :+:
                           CallCMD exp :+:
                           ThreadCMD :+:
                           ChanCMD exp)) ()

-- | Simplified compilation from 'ParProg' to C. Input/output is done via two external functions:
-- @source@ and @sink@.
compilePar
    :: ( EvalExp exp
       , CompExp exp
       , VarPred exp Bool
       , VarPred exp Int
       , VarPred exp inp
       , VarPred exp out
       , Typeable :< VarPred exp
       )
    => ParProg exp inp out () -> IO ()
compilePar = putStrLn . compileParStr

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
