{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Monadic front end for Zeldspar
module Frontend where



import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Data.Proxy

import Language.Embedded.Imperative (VarPred, CompExp (varExp), VarId)

import Zeldspar hiding ((>>>))
import qualified Zeldspar



instance Monoid (Program exp inp out)
  where
    mempty  = Return
    mappend = (>>:)

newtype Prog exp inp out a = Prog { unProg :: WriterT (Program exp inp out) (State VarId) a }
  deriving (Functor, Applicative, Monad, MonadState VarId, MonadWriter (Program exp inp out))

runProg :: Prog exp inp out a -> (a, Program exp inp out)
runProg = flip evalState 0 . runWriterT . unProg

stmt :: Statement exp inp out -> Prog exp inp out ()
stmt s = tell (s :> Return)

emit :: exp out -> Prog exp inp out ()
emit = stmt . Emit

receive :: VarPred exp inp => Ref inp -> Prog exp inp out ()
receive = stmt . Receive

newRef :: Prog exp inp out (Ref a)
newRef = do
    v <- get; put (v+1)
    return (Ref v)

(=:) :: VarPred exp a => Ref a -> exp a -> Prog exp inp out ()
v =: a = stmt $ v := a

getRef :: forall exp inp out a . (VarPred exp a, CompExp exp) => Ref a -> Prog exp inp out (exp a)
getRef (Ref v) = do
    s@(Ref w :: Ref a) <- newRef
    stmt (s := varExp v)
    return $ varExp w

confiscate :: Prog exp inp out a -> Prog exp inp out (a, Program exp inp out)
confiscate = censor (const mempty) . listen

loop :: Prog exp inp out () -> Prog exp inp out ()
loop p = do
    (_,prg) <- confiscate p
    tell $ Loop prg

data One = Fst | Snd

type family WhichOne a b
  where
    WhichOne a () = Fst
    WhichOne () b = Snd

type family SelectT one a b
  where
    SelectT Fst a b = a
    SelectT Snd a b = b

type OneOf a b = SelectT (WhichOne a b) a b

class Select one
  where
    select :: Proxy one -> a -> b -> SelectT one a b

instance Select Fst where select _ a _ = a
instance Select Snd where select _ _ b = b

(>>>) :: forall exp inp msg out a b . Select (WhichOne a b) =>
    Prog exp inp msg a -> Prog exp msg out b -> Prog exp inp out (OneOf a b)
Prog p1 >>> Prog p2 = Prog $ WriterT $ do
    (a,prog1) <- runWriterT p1
    (b,prog2) <- runWriterT p2
    return (select (Proxy :: Proxy (WhichOne a b)) a b, prog1 Zeldspar.>>> prog2)

