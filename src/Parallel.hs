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
-- | Parallel stream composition for Ziria.
module Parallel where
import Prelude hiding (break)
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

class Parallel p where
  type PExp p :: * -> *
  liftP :: p i o () -> ParProg (PExp p) i o ()

instance Parallel (ParProg exp) where
  type PExp (ParProg exp) = exp
  liftP = id

instance Parallel (Z exp) where
  type PExp (Z exp) = exp
  liftP = Lift

(|>>>|)
    :: ( Parallel l, exp ~ PExp l
       , Parallel r, exp ~ PExp r
       , VarPred exp i
       , VarPred exp x
       , VarPred exp o
       )
    => l i x () -> r x o () -> ParProg exp i o ()
l |>>>| r = liftP l :|>>>| liftP r

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

-- | Translate 'ParProg' to 'Program'.
--   The program terminates when either the source returns @(anything, False)@,
--   or the sink returns @False@.
translatePar
    :: ( EvalExp (IExp instr)
       , CompExp (IExp instr)
       , VarPred (IExp instr) Bool
       , VarPred (IExp instr) ChanBound
       , VarPred (IExp instr) inp
       , VarPred (IExp instr) out
       , Typeable :< VarPred (IExp instr)
       , RefCMD (IExp instr)     :<: instr
       , ControlCMD (IExp instr) :<: instr
       , ThreadCMD               :<: instr
       , ChanCMD (IExp instr)    :<: instr
       )
    => ParProg (IExp instr) inp out ()
    -> Program instr (IExp instr inp, IExp instr Bool)     -- ^ Source
    -> (IExp instr out -> Program instr (IExp instr Bool)) -- ^ Sink
    -> Program instr ()
translatePar ps inp out = do
    i <- newCloseableChan (litExp 10)
    o <- foldPP i ps $ \i p -> do
      o <- newCloseableChan (litExp 10)
      fork . void $ translate p (readC i o) (writeC i o)
      return o

    -- Read from output channel, shove output into sink
    lastthread <- fork $ do
      while (return $ litExp True) $ do
        x <- readChan o
        stillopen <- lastChanReadOK o
        iff stillopen (return ()) break
        outsideopen <- out x
        iff outsideopen (return ()) break
      closeChan o

    -- Read from source, shove into input channel
    while (return $ litExp True) $ do
      (x, outsideopen) <- inp
      iff outsideopen (return ()) break
      stillopen <- writeChan i x
      iff stillopen (return ()) break
    closeChan i
    waitThread lastthread
  where
    readC i o = do
      x <- readChan i
      stillopen <- lastChanReadOK i
      iff stillopen
        (return ())
        (closeChan o)
      return x
    writeC i o x = do
      stillopen <- writeChan o x
      iff stillopen
        (return ())
        (closeChan i)

-- | Simplified compilation from 'ParProg' to C. Input/output is done via two external functions:
-- @source@ and @sink@.
compileParStr :: forall exp inp out
    .  ( EvalExp exp
       , CompExp exp
       , VarPred exp Bool
       , VarPred exp ChanBound
       , VarPred exp inp
       , VarPred exp out
       , Typeable :< VarPred exp
       )
    => ParProg exp inp out () -> String
compileParStr prog =
    show $ prettyCGen $ liftSharedLocals $ wrapMain $ interpret $ cprog
  where
    src = do
      readokref <- initRef (litExp True)
      x <- callFun "source" [RefArg readokref]
      readok <- getRef readokref
      return (x, readok)
    snk o = callFun "sink" [FunArg o]
    cprog = translatePar prog src snk
              :: Program ((RefCMD exp :+:
                           ControlCMD exp :+:
                           FileCMD exp :+:
                           CallCMD exp :+:
                           ThreadCMD :+:
                           ChanCMD exp)) ()

-- | Simplified compilation from 'ParProg' to C.
--   Input/output is done via two external functions:
--   @$INPUT_TYPE source(int *)@ and @int sink($OUTPUT_TYPE)@.
compilePar
    :: ( EvalExp exp
       , CompExp exp
       , VarPred exp Bool
       , VarPred exp ChanBound
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
