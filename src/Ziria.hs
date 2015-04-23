{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | A generic implementation of sequential Ziria programs
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

-- | Ziria instructions
data ZiriaCMD exp inp out prog a
  where
    NewVar  :: VarPred exp a => ZiriaCMD exp inp out prog (Ref a)
    (:=)    :: VarPred exp a => Ref a -> exp a -> ZiriaCMD exp inp out prog ()
    Emit    :: exp out -> ZiriaCMD exp inp out prog ()
    Receive :: VarPred exp inp => Ref inp -> ZiriaCMD exp inp out prog ()
    Loop    :: prog () -> ZiriaCMD exp inp out prog ()
    EndL    :: prog () -> ZiriaCMD exp inp out prog ()

-- `ZiriaCMD` is purposefully designed so that most instructions return `()`. The reason is that
-- `unloop` needs to be able to rotate the body of a loop so that the first instruction is placed
-- before the loop and then again at the end of the body. This kind of rotating works well for
-- instructions without results, but it is generally not possible when result values are involved
-- (it doesn't make sense to bind a value at the end of a loop, since that value immediately goes
-- out of scope). The exception is `NewVar` which returns a `Ref`. In this particular case it is
-- possible for `unloop` to just put the instruction before the loop and not at the end of the body.
-- A new variable is created before the loop, and the same variable is reused throughout the loop.

instance MapInstr (ZiriaCMD exp inp out)
  where
    imap _ NewVar      = NewVar
    imap _ (v := a)    = v := a
    imap _ (Emit a)    = Emit a
    imap _ (Receive r) = Receive r
    imap f (Loop p)    = Loop (f p)
    imap f (EndL p)    = EndL (f p)

-- | The type of sequential Ziria programs
newtype Z exp inp out a = Z { unZ :: Program (ZiriaCMD exp inp out) a }
  deriving (Functor, Applicative, Monad)



----------------------------------------------------------------------------------------------------
-- * Interpretation
----------------------------------------------------------------------------------------------------

runCMD :: EvalExp exp => IO inp -> (out -> IO ()) -> ZiriaCMD exp inp out IO a -> IO a
runCMD src snk NewVar                = fmap RefEval $ newIORef (error "uninitialized reference")
runCMD src snk (RefEval r := a)      = writeIORef r $ evalExp a
runCMD src snk (Emit a)              = snk $ evalExp a
runCMD src snk (Receive (RefEval r)) = src >>= writeIORef r
runCMD src snk l@(Loop p)            = p >> runCMD src snk l

-- | Interpret a Ziria program in the 'IO' monad
runIO :: EvalExp exp => Z exp inp out a -> IO inp -> (out -> IO ()) -> IO a
runIO prog src snk = interpretWithMonad (runCMD src snk) $ unZ prog



----------------------------------------------------------------------------------------------------
-- * Compilation
----------------------------------------------------------------------------------------------------

transCMD
    :: ( EvalExp (IExp instr)
       , CompExp (IExp instr)
       , VarPred (IExp instr) Bool
       , RefCMD (IExp instr)     :<: instr
       , ControlCMD (IExp instr) :<: instr
       )
    => Program instr (IExp instr inp)        -- ^ Source
    -> (IExp instr out -> Program instr ())  -- ^ Sink
    -> ZiriaCMD (IExp instr) inp out (Program instr) a
    -> Program instr a
transCMD src snk NewVar      = newRef
transCMD src snk (r := a)    = setRef r a
transCMD src snk (Emit a)    = snk a
transCMD src snk (Receive r) = src >>= setRef r
transCMD src snk l@(Loop p)  = while (return $ litExp True) p

-- | Translate 'Z' to 'Program'
translate
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
translate prog src snk = interpretWithMonad (transCMD src snk) $ unZ prog

-- | Simplified compilation from 'Z' to C. Input/output is done via two external functions: @source@
-- and @sink@.
compileStr :: forall exp inp out a
    .  ( EvalExp exp
       , CompExp exp
       , VarPred exp Bool
       , VarPred exp inp
       )
    => Z exp inp out a -> String
compileStr prog = show $ prettyCGen $ wrapMain $ interpret cprog
  where
    src   = callFun "source" []
    snk   = \o -> callProc "sink" [FunArg o]
    cprog = translate prog src snk :: Program (RefCMD exp :+: ControlCMD exp :+: CallCMD exp) a

-- | Simplified compilation from 'Z' to C. Input/output is done via two external functions: @source@
-- and @sink@.
compile
    :: ( EvalExp exp
       , CompExp exp
       , VarPred exp Bool
       , VarPred exp inp
       )
    => Z exp inp out a -> IO ()
compile = putStrLn . compileStr



----------------------------------------------------------------------------------------------------
-- * Pipelining
----------------------------------------------------------------------------------------------------

-- | Program composition. The programs are always fused.
(>>>) :: Z exp inp msg () -> Z exp msg out () -> Z exp inp out ()
Z p >>> Z q = p ->>>- q
  where
    (->>>-) :: Program (ZiriaCMD exp inp msg) ()
            -> Program (ZiriaCMD exp msg out) ()
            -> Z exp inp out ()
    prog1 ->>>- prog2 = view prog1 .>>>. view prog2

    endl = singleton . EndL

    (.>>>.) :: ProgramView (ZiriaCMD exp inp msg) ()
            -> ProgramView (ZiriaCMD exp msg out) ()
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

unloop :: ProgramView (ZiriaCMD exp inp out) () -> Z exp inp out ()
unloop (NewVar        :>>= q) = newVar >>= \v -> loop (Z $ q v)  -- No need for newVar at the end
unloop (i@(_ := _)    :>>= q) = Z (singleton i) >> loop (Z (q () >> singleton i))
unloop (i@(Emit _)    :>>= q) = Z (singleton i) >> loop (Z (q () >> singleton i))
unloop (i@(Receive _) :>>= q) = Z (singleton i) >> loop (Z (q () >> singleton i))
unloop (Loop q        :>>= _) = unloop (view q)
  -- TODO It's assumed that `p /= Return a`. This could be captured in the type.

blockInp :: ProgramView (ZiriaCMD exp inp out) () -> Z exp xxx out ()
blockInp (NewVar    :>>= p) = newVar >>= \v -> blockInp (view $ p v)
blockInp ((v := e)  :>>= p) = (v =: e) >> blockInp (view $ p ())
blockInp (Emit m    :>>= p) = emit m >> blockInp (view $ p ())
blockInp (Receive _ :>>= _) = loop (return ())
blockInp (Loop p    :>>= _) = loop (blockInp $ view p)
blockInp (EndL p    :>>= q) = blockInp $ view $ unZ $ loop (Z p) >>= Z . q
blockInp (Return a)         = return a

blockOut :: ProgramView (ZiriaCMD exp inp out) () -> Z exp inp xxx ()
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

-- | Create an uninitialized variable
newVar :: VarPred exp a => Z exp inp out (Ref a)
newVar = Z $ singleton NewVar

-- | Create an initialized variable
initVar :: VarPred exp a => exp a -> Z exp inp out (Ref a)
initVar a = do
    v <- newVar
    v =: a
    return v

-- | Assign to a variable
(=:) :: VarPred exp a => Ref a -> exp a -> Z exp inp out ()
v =: a = Z $ singleton (v := a)

-- | Read a variable
readVar :: (VarPred exp a, EvalExp exp, CompExp exp) => Ref a -> Z exp inp out (exp a)
readVar v = do
    w <- newVar
    w =: unsafeFreezeRef v
    return $ unsafeFreezeRef w

-- | Emit a message to the output port
emit :: exp out -> Z exp inp out ()
emit = Z . singleton . Emit

-- | Receive a message from the input port
receiveVar :: VarPred exp inp => Ref inp -> Z exp inp out ()
receiveVar = Z . singleton . Receive

-- | Receive a message from the input port
receive :: (VarPred exp inp, EvalExp exp, CompExp exp) => Z exp inp out (exp inp)
receive = do
    v <- newVar
    receiveVar v
    return (unsafeFreezeRef v)

-- | Loop infinitely over the given program
loop :: Z exp inp out () -> Z exp inp out ()
loop = Z . singleton . Loop . unZ

