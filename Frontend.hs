{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Monadic front end
module Frontend where



import Control.Applicative
import Control.Monad.State
import Control.Monad.Writer

import Zeldspar



instance Monoid (Program var inp out ())
  where
    mempty  = Return
    mappend = (>>:)

newtype Prog var inp out a = Prog { unProg :: StateT Int (Writer (Program var inp out ())) a }
  deriving (Functor, Applicative, Monad, MonadState Int, MonadWriter (Program var inp out ()))

runProg :: Prog var inp out a -> Program var inp out ()
runProg = execWriter . flip runStateT 0 . unProg

stmt :: Statement var inp out -> Prog var inp out ()
stmt s = tell (s :> Return)

emit :: out -> Prog var inp out ()
emit = stmt . Emit

receive :: var inp -> Prog var inp out ()
receive = stmt . Receive

(=:) :: var a -> a -> Prog var inp out ()
v =: a = stmt $ v := a

confiscate :: Prog var inp out a -> Prog var inp out (a, Program var inp out ())
confiscate = censor (const mempty) . listen

loop :: Prog var inp out () -> Prog var inp out ()
loop p = do
    (_,prg) <- confiscate p
    tell $ Loop prg

endL :: Prog var inp out () -> Prog var inp out ()
endL p = do
    (_,prg) <- confiscate p
    tell $ EndL prg

