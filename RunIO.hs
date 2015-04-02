{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RunIO where



import Data.IORef

import Zeldspar



runProg :: forall inp out a . Program IORef inp out a -> IO inp -> (out -> IO ()) -> IO ()
runProg p get put = go p
  where
    go :: Program IORef inp out a -> IO ()
    go (Emit a    :> p) = put a >> go p
    go (Receive v :> p) = (get >>= writeIORef v) >> go p
    go (Fresh a   :> p) = newIORef a >> go p
    go (v := a    :> p) = writeIORef v a >> go p
    go (Loop p)         = go p >> go (Loop p)
    go Return           = return ()
    go (EndL p)         = go p >> go (EndL p)

