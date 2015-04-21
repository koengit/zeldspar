{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Ziria where

import Control.Applicative
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

newtype Z exp inp out a = Z { unZ :: Program (ZeldCMD exp inp out) a }
  deriving (Functor, Applicative, Monad)



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
runIO prog src snk = interpretWithMonad (runZeld src snk) $ unZ prog



----------------------------------------------------------------------------------------------------
-- * Compilation
----------------------------------------------------------------------------------------------------

compZeld
    :: ( EvalExp (IExp instr)
       , CompExp (IExp instr)
       , VarPred (IExp instr) Bool
       , RefCMD (IExp instr)     :<: instr
       , ControlCMD (IExp instr) :<: instr
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
       , VarPred (IExp instr) Bool
       , RefCMD (IExp instr)     :<: instr
       , ControlCMD (IExp instr) :<: instr
       )
    => Z (IExp instr) inp out a
    -> Program instr (IExp instr inp)        -- ^ Source
    -> (IExp instr out -> Program instr ())  -- ^ Sink
    -> Program instr a
compile prog src snk = interpretWithMonad (compZeld src snk) $ unZ prog

icompile :: forall instr exp inp out a
    .  ( EvalExp exp
       , CompExp exp
       , VarPred exp Bool
       , VarPred exp inp
       , instr ~ (RefCMD exp :+: ControlCMD exp :+: CallCMD exp)
       )
    => Z exp inp out a -> IO ()
icompile prog = print $ prettyCGen $ wrapMain $ interpret cprog
  where
    src   = callFun "source" []
    snk   = \o -> callProc "sink" [FunArg o]
    cprog = compile prog src snk :: Program instr a



----------------------------------------------------------------------------------------------------
-- * Pipelining
----------------------------------------------------------------------------------------------------

-- TODO Generalize result type of >>>

(>>>) :: Z exp inp msg () -> Z exp msg out () -> Z exp inp out ()
Z p >>> Z q = p ->>>- q
  where
    (->>>-) :: Program (ZeldCMD exp inp msg) ()
            -> Program (ZeldCMD exp msg out) ()
            -> Z exp inp out ()
    prog1 ->>>- prog2 = view prog1 .>>>. view prog2

    endl = singleton . EndL

    (.>>>.) :: ProgramView (ZeldCMD exp inp msg) ()
            -> ProgramView (ZeldCMD exp msg out) ()
            -> Z exp inp out ()

    -- termination
    (Return a) .>>>. _          = return a
    _          .>>>. (Return b) = return b

    -- variables
    (NewVar :>>= p) .>>>. q               = newVar >>= \v -> p v ->>>- unview q
    p               .>>>. (NewVar :>>= q) = newVar >>= \v -> unview p ->>>- q v

    ((v := a) :>>= p) .>>>. q                 = (v =: a) >> (p () ->>>- unview q)
    p                 .>>>. ((v := a) :>>= q) = (v =: a) >> (unview p ->>>- q ())

    -- connect
    (Emit m :>>= p) .>>>. (Receive v :>>= q) = (v =: m) >> (p () ->>>- q ())

    -- loop
    (Loop p :>>= _) .>>>. (Loop q :>>= _) = loop ((p >> endl p) ->>>- (q >> endl q))

    (Loop p :>>= _) .>>>. q = case view p of
                                Return _ -> blockInp q
                                p'       -> unloop p' >>> Z (unview q)

    p .>>>. (Loop q :>>= _) = case view q of
                                Return _ -> blockOut p
                                q'       -> Z (unview p) >>> unloop q'

    -- outside actions
    (Receive v :>>= p) .>>>. q               = receiveVar v >> (p () ->>>- unview q)
    p                  .>>>. (Emit m :>>= q) = emit m       >> (unview p ->>>- q ())

    -- end loop
    (EndL _ :>>= _) .>>>. (EndL _ :>>= _) = return ()
    (EndL p :>>= _) .>>>. q               = (p >> singleton (EndL p)) ->>>- unview q
    p               .>>>. (EndL q :>>= _) = unview p ->>>- (q >> singleton (EndL q))

unloop :: ProgramView (ZeldCMD exp inp out) () -> Z exp inp out ()
unloop (NewVar        :>>= q) = newVar >>= \v -> loop (Z $ q v)  -- No need for NewVar at the end
unloop (i@(_ := _)    :>>= q) = Z (singleton i) >> loop (Z (q () >> singleton i))
unloop (i@(Emit _)    :>>= q) = Z (singleton i) >> loop (Z (q () >> singleton i))
unloop (i@(Receive _) :>>= q) = Z (singleton i) >> loop (Z (q () >> singleton i))
unloop (Loop q        :>>= _) = unloop (view q)
  -- TODO It's assumed that `p /= Return a`. This could be captured in the type.

blockInp :: ProgramView (ZeldCMD exp inp out) () -> Z exp xxx out ()
blockInp (NewVar    :>>= p) = newVar >>= \v -> blockInp (view $ p v)
blockInp ((v := e)  :>>= p) = (v =: e) >> blockInp (view $ p ())
blockInp (Emit m    :>>= p) = emit m >> blockInp (view $ p ())
blockInp (Receive _ :>>= _) = loop (return ())
blockInp (Loop p    :>>= _) = loop (blockInp $ view p)
blockInp (EndL p    :>>= q) = blockInp $ view $ unZ $ loop (Z p) >>= Z . q
blockInp (Return a)         = return a

blockOut :: ProgramView (ZeldCMD exp inp out) () -> Z exp inp xxx ()
blockOut (NewVar    :>>= p) = newVar >>= \v -> blockOut (view $ p v)
blockOut ((v := e)  :>>= p) = (v =: e) >> blockOut (view $ p ())
blockOut (Emit _    :>>= _) = loop (return ())
blockOut (Receive v :>>= p) = receiveVar v >> blockOut (view $ p ())
blockOut (Loop p    :>>= _) = loop (blockOut $ view p)
blockOut (EndL p    :>>= q) = blockOut $ view $ unZ $ loop (Z p) >>= Z. q
blockOut (Return a)         = return a



----------------------------------------------------------------------------------------------------
-- * Front end
----------------------------------------------------------------------------------------------------

newVar :: VarPred exp a => Z exp inp out (Ref a)
newVar = Z $ singleton NewVar

initVar :: VarPred exp a => exp a -> Z exp inp out (Ref a)
initVar a = do
    v <- newVar
    v =: a
    return v

(=:) :: VarPred exp a => Ref a -> exp a -> Z exp inp out ()
v =: a = Z $ singleton (v := a)

readVar :: (VarPred exp a, EvalExp exp, CompExp exp) => Ref a -> Z exp inp out (exp a)
readVar v = do
    w <- newVar
    w =: unsafeFreezeRef v
    return $ unsafeFreezeRef w

emit :: exp out -> Z exp inp out ()
emit = Z . singleton . Emit

receiveVar :: VarPred exp inp => Ref inp -> Z exp inp out ()
receiveVar = Z . singleton . Receive

receive :: (VarPred exp inp, EvalExp exp, CompExp exp) => Z exp inp out (exp inp)
receive = do
    v <- newVar
    receiveVar v
    return (unsafeFreezeRef v)

loop :: Z exp inp out () -> Z exp inp out ()
loop = Z . singleton . Loop . unZ

