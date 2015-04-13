{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | This module defines a simple expression type for use in Zeldspar, and a function for running
-- Zeldspar programs in the 'IO' monad.
module RunIO where



import Control.Monad.Identity
import Control.Monad.State
import Data.Dynamic
import Data.Map (Map)
import qualified Data.Map as Map

import Zeldspar



data Exp a
  where
    Var :: Typeable a => VarId -> Exp a
    Lit :: a -> Exp a
    Op1 :: String -> (a -> b) -> Exp a -> Exp b
    Op2 :: String -> (a -> b -> c) -> Exp a -> Exp b -> Exp c

instance VarExp Exp where varExp = Var

instance EvalExp Exp Run
  where
    eval (Var v) = gets (fromD . (Map.! v))
      where
        fromD d = case fromDynamic d of
            Nothing -> error "eval: type error"
            Just a  -> a
    eval (Lit a)       = return a
    eval (Op1 _ f a)   = liftM f $ eval a
    eval (Op2 _ f a b) = liftM2 f (eval a) (eval b)

instance (Num a, Typeable a) => Num (Exp a)
  where
    fromInteger = Lit . fromInteger
    (+)         = Op2 "(+)" (+)
    (-)         = Op2 "(-)" (-)
    (*)         = Op2 "(*)" (*)
    abs         = Op1 "abs" abs
    signum      = Op1 "signum" signum

type Store = Map Int Dynamic
type Run   = StateT Store IO

assign :: Ref a -> Exp a -> Run ()
assign (Ref v) = modify . Map.insert v . toDyn <=< eval

assignRef :: Ref a -> Ref a -> Run ()
assignRef (Ref v) (Ref w) = do
    store <- get
    modify $ Map.insert v (store Map.! w)

runIO :: forall inp out . Program Exp inp out -> IO inp -> (out -> IO ()) -> IO ()
runIO p get put = flip evalStateT Map.empty $ go p
  where
    go :: Program Exp inp out -> Run ()
    go (Emit a    :> p) = (liftIO . put =<< eval a) >> go p
    go (Receive r :> p) = (assign r . Lit =<< liftIO get) >> go p
    go (r := a    :> p) = assign r a >> go p
    go (r :== a   :> p) = assignRef r a >> go p
    go (Loop p)         = go p >> go (Loop p)
    go Return           = return ()
    go (EndL p)         = go p >> go (EndL p)

