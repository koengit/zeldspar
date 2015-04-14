{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Monadic front end for Zeldspar
module Frontend where



import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer
import Data.Typeable

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

receive :: Ref inp -> Prog exp inp out ()
receive = stmt . Receive

newRef :: Typeable a => Prog exp inp out (Ref a)
newRef = do
    v <- get; put (v+1)
    return (Ref v)

(=:) :: Ref a -> exp a -> Prog exp inp out ()
v =: a = stmt $ v := a

getRef :: (Typeable a, VarExp exp) => Ref a -> Prog exp inp out (exp a)
getRef r = do
    s@(Ref w) <- newRef
    stmt (s :== r)
    return $ varExp w

confiscate :: Prog exp inp out a -> Prog exp inp out (a, Program exp inp out)
confiscate = censor (const mempty) . listen

loop :: Prog exp inp out () -> Prog exp inp out ()
loop p = do
    (_,prg) <- confiscate p
    tell $ Loop prg

endL :: Prog exp inp out () -> Prog exp inp out ()
endL p = do
    (_,prg) <- confiscate p
    tell $ EndL prg

(>>>) :: Prog exp inp msg a -> Prog exp msg out b -> Prog exp inp out (a,b)
Prog p1 >>> Prog p2 = Prog $ WriterT $ do
    (a,prog1) <- runWriterT p1
    (b,prog2) <- runWriterT p2
    return ((a,b), prog1 Zeldspar.>>> prog2)

