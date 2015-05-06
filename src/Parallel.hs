{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Parallel stream composition for Ziria.
module Parallel where

import Prelude hiding (break)

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad
import Control.Monad.IO.Class ()
#if __GLASGOW_HASKELL__ < 708
import Data.Typeable
#endif

import Language.Embedded.Expression
import Language.Embedded.Imperative hiding (compile)
import qualified Language.Embedded.Imperative as Imp
import Language.Embedded.Concurrent

import Ziria

import qualified Control.Concurrent as CC

-- TODO: allow returning values?
data ParProg exp i o a where
  LiftP    :: Z exp i o ()
           -> ParProg exp i o ()

  (:|>>>|) :: (VarPred exp i, VarPred exp o, VarPred exp t)
           => ParProg exp i t ()
           -> ParProg exp t o ()
           -> ParProg exp i o ()

class Parallel p where
  type PExp p :: * -> *
  -- | Lift a computation to the parallel level.
  liftP :: p i o () -> ParProg (PExp p) i o ()

instance Parallel (ParProg exp) where
  type PExp (ParProg exp) = exp
  liftP = id

instance Parallel (Z exp) where
  type PExp (Z exp) = exp
  liftP = LiftP

data CloseChan a = Chan (CC.Chan (Maybe a)) (CC.MVar Bool)

newCC :: IO (CloseChan a)
newCC = Chan <$> CC.newChan <*> CC.newMVar False

readCC :: CloseChan a -> IO (Maybe a)
readCC (Chan ch cl) = do
  mx <- CC.readChan ch
  case mx of
    Just x -> return (Just x)
    _      -> CC.writeChan ch Nothing >> return Nothing

writeCC :: CloseChan a -> a -> IO Bool
writeCC (Chan ch cl) x = do
  closed <- CC.readMVar cl
  if closed
    then return False
    else CC.writeChan ch (Just x) >> return True

closeCC :: CloseChan a -> IO ()
closeCC (Chan ch cl) = void $ do
  CC.swapMVar cl True
  CC.writeChan ch Nothing

(|>>>|) :: (Parallel a, exp ~ PExp a,
            Parallel b, exp ~ PExp b,
            VarPred exp i,
            VarPred exp t,
            VarPred exp o)
        => a i t ()
        -> b t o ()
        -> ParProg exp i o ()
l |>>>| r = liftP l :|>>>| liftP r

-- | Interpret 'ParProg' in the 'IO' monad
runPar :: (EvalExp exp,
           VarPred exp i,
           VarPred exp o,
           VarPred exp Bool,
           VarPred exp ChanBound)
       => ParProg exp i o ()
       -> IO i
       -> (o -> IO ())
       -> IO ()
runPar ps inp out = do
    i <- newCC
    o <- foldPP i ps $ \i p -> do
      o <- newCC
      CC.forkIO . void $ do
        runIO p (readC i o) (writeC i o)
        closeCC i
        closeCC o
      return o
    CC.forkIO . forever $ readC o o >>= out
    sourceTo i
  where
    sourceTo i = do
      ok <- inp >>= writeCC i
      when ok $ sourceTo i
    readC i o = do
      mx <- readCC i
      case mx of
        Just x -> return x
        _      -> closeCC o >> error "[Thread self-terminated]"
    writeC i o x = do
      ok <- writeCC o x
      when (not ok) $ do
        closeCC i
        error "[Thread self-terminated]"

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
      forkWithId $ \t -> void $ do
        translate p (readC t i o) (writeC t i o)
        closeChan i
        closeChan o
      return o

    -- Read from output channel, shove output into sink
    -- TODO: inline into the last thread
    lastthread <- forkWithId $ \t -> do
      while (return $ litExp True) $ do
        x <- readChan o
        stillopen <- lastChanReadOK o
        iff stillopen (return ()) (killThread t)
        outsideopen <- out x
        iff outsideopen (return ()) (closeChan o >> killThread t)

    -- Read from source, shove into input channel
    -- TODO: inline into the first thread
    while (return $ litExp True) $ do
      (x, outsideopen) <- inp
      iff outsideopen (return ()) break
      stillopen <- writeChan i x
      iff stillopen (return ()) break
    closeChan i
    waitThread lastthread
  where
    readC t i o = do
      x <- readChan i
      stillopen <- lastChanReadOK i
      iff stillopen
        (return ())
        (closeChan o >> killThread t)
      return x
    writeC t i o x = do
      stillopen <- writeChan o x
      iff stillopen
        (return ())
        (closeChan i >> killThread t)

-- | Simplified compilation from 'ParProg' to C. Input/output is done via two external functions:
-- @receive@ and @emit@.
compilePar :: forall exp inp out
    .  ( EvalExp exp
       , CompExp exp
       , VarPred exp Bool
       , VarPred exp ChanBound
       , VarPred exp inp
       , VarPred exp out
       )
    => ParProg exp inp out () -> String
compilePar prog = Imp.compile $ cprog
  where
    src = do
      readokref <- initRef (litExp True)
      x <- externFun "receive" [RefArg readokref]
      readok <- getRef readokref
      return (x, readok)
    snk o = externFun "emit" [ValArg o]
    cprog = translatePar prog src snk
              :: Program ((RefCMD exp :+:
                           ControlCMD exp :+:
                           FileCMD exp :+:
                           CallCMD exp :+:
                           ThreadCMD :+:
                           ChanCMD exp)) ()

-- | Simplified compilation from 'ParProg' to C.
--   Input/output is done via two external functions:
--   @$INPUT_TYPE receive(int *)@ and @int emit($OUTPUT_TYPE)@.
icompilePar
    :: ( EvalExp exp
       , CompExp exp
       , VarPred exp Bool
       , VarPred exp ChanBound
       , VarPred exp inp
       , VarPred exp out
       )
    => ParProg exp inp out () -> IO ()
icompilePar = putStrLn . compilePar

-- | Left fold over a 'ParProg'.
foldPP :: (Monad m,
           VarPred exp i,
           VarPred exp o)
       => chan i
       -> ParProg exp i o a
       -> (forall i o a. (VarPred exp i, VarPred exp o)
           => chan i
           -> Z exp i o a
           -> m (chan o))
       -> m (chan o)
foldPP acc (LiftP p) f =
  f acc p
foldPP acc (a :|>>>| b) f =
  foldPP acc a f >>= \acc' -> foldPP acc' b f
