-- | Parallel stream composition for Zeldspar
module Zeldspar.Parallel where

import Prelude hiding (break)

import Feldspar.Run
import Feldspar.Run.Concurrent
import Zeldspar
import Ziria
import Ziria.Parallel


--------------------------------------------------------------------------------
-- * Representation
--------------------------------------------------------------------------------

type ParZun inp out = ParZ inp out Run


--------------------------------------------------------------------------------
-- * Translation
--------------------------------------------------------------------------------
{-
translatePar :: forall inp out a. (Type inp, Type out)
             => ParZun (Data inp) (Data out) a
             -> (Run (Data inp, Data Bool))    -- ^ Source
             -> (Data out -> Run (Data Bool))  -- ^ Sink
             -> Run a
translatePar ps inp out = do
    i <- newCloseableChan (value 10)
    o <- foldParZ i ps $ \i p -> do
      o <- newCloseableChan (value 10)
      forkWithId $ \t -> void $ do
        translate p (readC t i o) (writeC t i o)
        closeChan i
        closeChan o
      return o

    -- Read from output channel, shove output into sink
    -- TODO: inline into the last thread
    lastThread <- forkWithId $ \t -> do
      while (return true) $ do
        x <- readChan o
        stillopen <- lastChanReadOK o
        iff stillopen (return ()) (killThread t)
        outsideOpen <- out x
        iff outsideOpen (return ()) (closeChan o >> killThread t)

    -- Read from source, shove into input channel
    -- TODO: inline into the first thread
    while (return true) $ do
      (x, outsideOpen) <- inp
      iff outsideOpen (return ()) break
      stillopen <- writeChan i x
      iff stillopen (return ()) break
    closeChan i
    waitThread lastThread
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
-}
