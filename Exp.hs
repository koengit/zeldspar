{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Exp where



import Control.Monad.State
import Data.Dynamic
import qualified Data.Map as Map

import Zeldspar
import RunIO



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

