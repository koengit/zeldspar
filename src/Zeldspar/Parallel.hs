-- | Parallel stream composition for Zeldspar
module Zeldspar.Parallel
  ( module F
  , module Z
  , ParZun
  , translatePar
  ) where

import Prelude hiding (break)

import Feldspar.Run.Concurrent as F
import Zeldspar as Z
import Ziria as Z
import Ziria.Parallel as Z


--------------------------------------------------------------------------------
-- * Representation
--------------------------------------------------------------------------------

type ParZun inp out = ParZ inp out Run


--------------------------------------------------------------------------------
-- * Translation
--------------------------------------------------------------------------------

translatePar :: forall inp out. (Transferable inp, Transferable out)
             => Length                    -- ^ Default channel size
             -> ParZun inp out ()
             -> (Run (inp, Data Bool))    -- ^ Source
             -> (out -> Run (Data Bool))  -- ^ Sink
             -> Run ()
translatePar dchs ps inp out = do
    i <- newChan (value dchs)
    o <- foldParZ dchs i ps $ \chs i p -> do
      o <- newChan (value chs)
      forkWithId $ \t -> void $ do
        translate (p >> return ()) (readC t i o) (writeC t i o)
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

-- | Left fold over a 'ParZ'
foldParZ :: (Monad m, Transferable inp, Transferable out)
         => Length
         -> c inp
         -> ParZ inp out m a
         -> (forall inp out a. (Transferable inp, Transferable out)
             => Length -> c inp -> Z inp out m a -> m (c out))
         -> m (c out)
foldParZ chs acc (LiftP p)     f = f chs acc p
foldParZ chs acc (ConnP s a b) f =
    let chs' = maybe chs fromIntegral s
    in  foldParZ chs' acc a f >>= \acc' -> foldParZ chs acc' b f
