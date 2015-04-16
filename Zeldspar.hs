{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Zeldspar where

import Data.IORef

import Language.C.Monad
import Language.Embedded.Backend.C ()
import Language.Embedded.Imperative

infix  6 :=
infixr 4 >>>



----------------------------------------------------------------------------------------------------
-- * Representation
----------------------------------------------------------------------------------------------------

data ZeldCMD exp inp out prog a
  where
    NewVar  :: VarPred exp a => ZeldCMD exp inp out prog (Ref a)
    (:=)    :: VarPred exp a => Ref a -> exp a -> ZeldCMD exp inp out prog ()
    Emit    :: exp out -> ZeldCMD exp inp out prog ()
    Receive :: VarPred exp inp => Ref inp -> ZeldCMD exp inp out prog ()
    Loop    :: prog () -> ZeldCMD exp inp out prog ()
    EndL    :: prog () -> ZeldCMD exp inp out prog ()

instance MapInstr (ZeldCMD exp inp out)
  where
    imap _ NewVar      = NewVar
    imap _ (v := a)    = v := a
    imap _ (Emit a)    = Emit a
    imap _ (Receive r) = Receive r
    imap f (Loop p)    = Loop (f p)
    imap f (EndL p)    = EndL (f p)

type Z exp inp out = Program (ZeldCMD exp inp out)



----------------------------------------------------------------------------------------------------
-- * Interpretation
----------------------------------------------------------------------------------------------------

runZeld :: EvalExp exp => IO inp -> (out -> IO ()) -> ZeldCMD exp inp out IO a -> IO a
runZeld src snk NewVar                = fmap RefEval $ newIORef (error "uninitialized reference")
runZeld src snk (RefEval r := a)      = writeIORef r $ evalExp a
runZeld src snk (Emit a)              = snk $ evalExp a
runZeld src snk (Receive (RefEval r)) = src >>= writeIORef r
runZeld src snk l@(Loop p)            = p >> runZeld src snk l

runIO :: EvalExp exp => Z exp inp out a -> IO inp -> (out -> IO ()) -> IO a
runIO prog src snk = interpretWithMonad (runZeld src snk) prog



----------------------------------------------------------------------------------------------------
-- * Compilation
----------------------------------------------------------------------------------------------------

compZeld
    :: ( EvalExp (IExp instr)
       , CompExp (IExp instr)
       , VarPred (IExp instr) ~ IPred instr
       , IPred instr Bool
       , RefCMD (IPred instr) (IExp instr) :<: instr
       , ControlCMD (IExp instr)           :<: instr
       )
    => Program instr (IExp instr inp)        -- ^ Source
    -> (IExp instr out -> Program instr ())  -- ^ Sink
    -> ZeldCMD (IExp instr) inp out (Program instr) a
    -> Program instr a
compZeld src snk NewVar      = newRef
compZeld src snk (r := a)    = setRef r a
compZeld src snk (Emit a)    = snk a
compZeld src snk (Receive r) = src >>= setRef r
compZeld src snk l@(Loop p)  = while (return $ litExp True) p

compile
    :: ( EvalExp (IExp instr)
       , CompExp (IExp instr)
       , VarPred (IExp instr) ~ IPred instr
       , IPred instr Bool
       , RefCMD (IPred instr) (IExp instr) :<: instr
       , ControlCMD (IExp instr)           :<: instr
       )
    => Z (IExp instr) inp out a
    -> Program instr (IExp instr inp)        -- ^ Source
    -> (IExp instr out -> Program instr ())  -- ^ Sink
    -> Program instr a
compile prog src snk = interpretWithMonad (compZeld src snk) prog

icompile
    :: forall instr exp a . ( EvalExp exp
       , CompExp exp
       , VarPred exp Bool
       , VarPred exp Float
       , instr ~ (RefCMD (VarPred exp) exp :+: ControlCMD exp :+: FileCMD exp)
       )
    => Z exp Float Float a -> IO ()
icompile prog = print =<< prettyCGen (wrapMain $ interpret cprog)
  where
    cprog = do
        inp <- open "stdin"
        out <- open "stdout"
        compile prog (fget inp) (fput out) :: Program instr a



----------------------------------------------------------------------------------------------------
-- * Pipelining
----------------------------------------------------------------------------------------------------

pattern Ret a   <- (view -> Return a)
pattern i :>= p <- (view -> i :>>= p)

(|>=) :: instr (Program instr) a -> (a -> Program instr b) -> Program instr b
i |>= p = singleton i >>= p
  -- TODO It should be possible to use `:>=` for this using `where` syntax. Maybe that only works in
  --      GHC 7.10?

(|>>) :: instr (Program instr) a -> Program instr b -> Program instr b
i |>> p = singleton i >> p

(>>|) :: Program instr a -> instr (Program instr) b -> Program instr b
p >>| i = p >> singleton i

-- TODO Generalize result type of >>>

(>>>) :: Z exp inp msg () -> Z exp msg out () -> Z exp inp out ()

-- termination
(Ret a) >>> _       = return a
_       >>> (Ret b) = return b

-- variables
(NewVar :>= p) >>> q              = NewVar |>= \v -> p v >>> q
p              >>> (NewVar :>= q) = NewVar |>= \v -> p >>> q v

((v := a) :>= p) >>> q                = (v := a) |>> (p () >>> q)
p                >>> ((v := a) :>= q) = (v := a) |>> (p >>> q ())

-- connect
(Emit m :>= p) >>> (Receive v :>= q) = (v := m) |>> (p () >>> q ())

-- loop
(Loop p :>= _) >>> (Loop q :>= _) = singleton $ Loop ((p >>| EndL p) >>> (q >>| EndL q))

(Loop (Ret _) :>= _) >>> q = blockInp q
(Loop p       :>= _) >>> q = unloop p >>> q

p >>> (Loop (Ret _) :>= _) = blockOut p
p >>> (Loop q       :>= _) = p >>> unloop q

-- outside actions
(Receive v :>= p) >>> q              = Receive v |>> (p () >>> q)
p                 >>> (Emit m :>= q) = Emit m    |>> (p >>> q ())

-- end loop
(EndL _ :>= _) >>> (EndL _ :>= _) = return ()
(EndL p :>= _) >>> q              = (p >>| EndL p) >>> q
p              >>> (EndL q :>= _) = p >>> (q >>| EndL q)

unloop :: Z exp inp out () -> Z exp inp out ()
unloop (NewVar        :>= q) = NewVar |>= \v -> singleton (Loop (q v))  -- No need for NewVar at the end
unloop (i@(_ := _)    :>= q) = i |>> singleton (Loop (q () >>| i))
unloop (i@(Emit _)    :>= q) = i |>> singleton (Loop (q () >>| i))
unloop (i@(Receive _) :>= q) = i |>> singleton (Loop (q () >>| i))
unloop (Loop q        :>= _) = unloop q
  -- TODO It's assumed that `p /= Ret a`. This could be captured in the type.

blockInp :: Z exp inp out () -> Z exp xxx out ()
blockInp (NewVar    :>= p) = NewVar    |>= \v -> blockInp (p v)
blockInp ((v := e)  :>= p) = (v := e)  |>> blockInp (p ())
blockInp (Emit m    :>= p) = Emit m    |>> blockInp (p ())
blockInp (Receive _ :>= _) = singleton $ Loop (return ())
blockInp (Loop p    :>= _) = singleton $ Loop (blockInp p)
blockInp (EndL p    :>= q) = blockInp (Loop p |>= q)
blockInp (Ret a)           = return a

blockOut :: Z exp inp out () -> Z exp inp xxx ()
blockOut (NewVar    :>= p) = NewVar    |>= \v -> blockOut (p v)
blockOut ((v := e)  :>= p) = (v := e)  |>> blockOut (p ())
blockOut (Emit _    :>= _) = singleton $ Loop (return ())
blockOut (Receive v :>= p) = Receive v |>> blockOut (p ())
blockOut (Loop p    :>= _) = singleton $ Loop (blockOut p)
blockOut (EndL p    :>= q) = blockOut (Loop p |>= q)
blockOut (Ret a)           = return a



----------------------------------------------------------------------------------------------------
-- * Front end
----------------------------------------------------------------------------------------------------

newVar :: VarPred exp a => Z exp inp out (Ref a)
newVar = singleton NewVar

initVar :: VarPred exp a => exp a -> Z exp inp out (Ref a)
initVar a = do
    v <- newVar
    v =: a
    return v

(=:) :: VarPred exp a => Ref a -> exp a -> Z exp inp out ()
v =: a = singleton (v := a)

readVar :: (VarPred exp a, EvalExp exp, CompExp exp) => Ref a -> Z exp inp out (exp a)
readVar v = do
    w <- newVar
    w =: unsafeFreezeRef v
    return $ unsafeFreezeRef w

emit :: exp out -> Z exp inp out ()
emit = singleton . Emit

receiveVar :: VarPred exp inp => Ref inp -> Z exp inp out ()
receiveVar = singleton . Receive

receive :: (VarPred exp inp, EvalExp exp, CompExp exp) => Z exp inp out (exp inp)
receive = do
    v <- newVar
    receiveVar v
    return (unsafeFreezeRef v)

loop :: Z exp inp out () -> Z exp inp out ()
loop = singleton . Loop

