{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | This module defines a simple expression type for use in Zeldspar, and a function for running
-- Zeldspar programs in the 'IO' monad.
module RunIO where



import Control.Monad.State
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as Map

import Language.Embedded.Imperative (Dict (..), (:<) (..), VarPred, VarId)
import Language.Embedded.Expr

import Zeldspar
import Frontend



-- | Interface for evaluating expressions
class EvalExpM exp m
  where
    eval :: exp a -> m a

instance EvalExpM Expr Run
  where
    eval a = do
        store <- get
        return $ evalExpr (store Map.!) a

type Store = Map VarId Dynamic
type Run   = StateT Store IO

assign :: Typeable a => Ref a -> a -> Run ()
assign (Ref v) = modify . Map.insert v . toDyn

withTypeable :: forall exp a b . (VarPred exp a, Typeable :< VarPred exp) =>
    Proxy exp -> Ref a -> (Typeable a => b) -> b
withTypeable _ _ b = case sub (Dict :: Dict (VarPred exp a)) of
    (Dict :: Dict (Typeable a)) -> b

runIO :: forall exp inp out a . (EvalExpM exp Run, Typeable :< VarPred exp) =>
    Prog exp inp out a -> IO inp -> (out -> IO ()) -> IO a
runIO prog get put = do
    let (a,p) = runProg prog
    flip evalStateT Map.empty $ go p
    return a
  where
    go :: Program exp inp out -> Run ()
    go (Emit a    :> p) = (eval a >>= liftIO . put) >> go p
    go (Receive r :> p) = withTypeable (Proxy :: Proxy exp) r $
                            (liftIO get >>= assign r) >> go p
    go (r := a    :> p) = withTypeable (Proxy :: Proxy exp) r $
                            (eval a >>= assign r) >> go p
    go (Loop p)         = go p >> go (Loop p)
    go Return           = return ()
    -- EndL should not appear here

