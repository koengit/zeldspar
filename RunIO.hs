{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module defines a simple expression type for use in Zeldspar, and a function for running
-- Zeldspar programs in the 'IO' monad.
module RunIO where



import Control.Monad.State
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as Map

import Zeldspar
import Frontend



type Store = Map VarId Dynamic
type Run   = StateT Store IO

assign :: Ref a -> a -> Run ()
assign (Ref v) = modify . Map.insert v . toDyn

assignRef :: Ref a -> Ref a -> Run ()
assignRef (Ref v) (Ref w) = do
    store <- get
    modify $ Map.insert v (store Map.! w)

runIO :: forall exp inp out . EvalExp exp Run =>
    Prog exp inp out () -> IO inp -> (out -> IO ()) -> IO ()
runIO p get put = flip evalStateT Map.empty $ go $ runProg p
  where
    go :: Program exp inp out -> Run ()
    go (Emit a    :> p) = (eval a >>= liftIO . put) >> go p
    go (Receive r :> p) = (liftIO get >>= assign r) >> go p
    go (r := a    :> p) = (eval a >>= assign r) >> go p
    go (r :== a   :> p) = assignRef r a >> go p
    go (Loop p)         = go p >> go (Loop p)
    go Return           = return ()
    go (EndL p)         = go p >> go (EndL p)

