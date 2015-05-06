{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | An implementation of Ziria that uses Feldspar to represent pure computations
module Zeldspar where



#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif

import Language.Embedded.Imperative
import Language.Embedded.Concurrent

import Feldspar hiding (Ref)
import Feldspar.Compiler.FromImperative ()

import qualified Ziria
import Parallel (Parallel (..))
import qualified Parallel as Ziria



-- | The type of sequential Zeldspar programs
newtype Z inp out a = Z { unZ :: Ziria.Z Data inp out a }
  deriving (Functor, Applicative, Monad)

newtype ParZ inp out a = ParZ { unParZ :: Ziria.ParProg Data inp out a }

instance Ziria.Parallel Z
  where
    type PExp Z = Data
    liftP = liftP . unZ

instance Ziria.Parallel ParZ
  where
    type PExp ParZ = Data
    liftP = liftP . unParZ

-- | Interpret a Zeldspar program in the 'IO' monad
runZ
    :: Z inp out a
    -> IO inp          -- ^ Source
    -> (out -> IO ())  -- ^ Sink
    -> IO a
runZ = Ziria.runIO . unZ

-- | Interpret a parallel Zeldspar program in the 'IO' monad
runParZ
    :: (Type inp, Type out)
    => ParZ inp out ()
    -> IO inp          -- ^ Source
    -> (out -> IO ())  -- ^ Sink
    -> IO ()
runParZ = Ziria.runPar . unParZ

-- | Translate 'Z' to 'Program'
translate
    :: ( RefCMD Data     :<: instr
       , ControlCMD Data :<: instr
       , IExp instr ~ Data
       )
    => Z inp out a
    -> Program instr (Data inp)        -- ^ Source
    -> (Data out -> Program instr ())  -- ^ Sink
    -> Program instr a
translate = Ziria.translate . unZ

-- | Translate 'ParZ' to 'Program'
translatePar
    :: ( Type Bool
       , Type ChanBound
       , Type inp
       , Type out
       , RefCMD (IExp instr)     :<: instr
       , ControlCMD (IExp instr) :<: instr
       , ThreadCMD               :<: instr
       , ChanCMD (IExp instr)    :<: instr
       , IExp instr ~ Data
       )
    => ParZ inp out ()
    -> Program instr (Data inp, IExp instr Bool)     -- ^ Source
    -> (Data out -> Program instr (IExp instr Bool)) -- ^ Sink
    -> Program instr ()
translatePar = Ziria.translatePar . unParZ

-- | Simplified compilation from 'Z' to C. Input/output is done via two external functions:
-- @receive@ and @emit@.
compile :: (Type inp, Type out) => Z inp out a -> String
compile = Ziria.compile . unZ

-- | Simplified compilation from 'ParZ' to C. Input/output is done via two external functions:
-- @receive@ and @emit@.
compilePar :: (Type ChanBound, Type inp, Type out) => ParZ inp out () -> String
compilePar = Ziria.compilePar . unParZ

-- | Simplified compilation from 'Z' to C. Input/output is done via two external functions:
-- @receive@ and @emit@.
icompile :: (Type inp, Type out) => Z inp out a -> IO ()
icompile = Ziria.icompile . unZ

-- | Simplified compilation from 'ParZ' to C. Input/output is done via two external functions:
-- @receive@ and @emit@.
icompilePar :: (Type ChanBound, Type inp, Type out) => ParZ inp out () -> IO ()
icompilePar = Ziria.icompilePar . unParZ

-- | Program composition. The programs are always fused.
(>>>) :: Z inp msg () -> Z msg out () -> Z inp out ()
Z p1 >>> Z p2 = Z (p1 Ziria.>>> p2)

-- | Parallel program composition
(|>>>|)
    :: ( Type i
       , Type x
       , Type o
       , Parallel a, PExp a ~ Data
       , Parallel b, PExp b ~ Data
       )
    => a i x () -> b x o () -> ParZ i o ()
p1 |>>>| p2 = ParZ (p1 Ziria.|>>>| p2)

-- | Create an uninitialized variable
newVar :: Type a => Z inp out (Ref a)
newVar = Z Ziria.newVar

-- | Create an initialized variable
initVar :: Type a => Data a -> Z inp out (Ref a)
initVar = Z . Ziria.initVar

-- | Assign to a variable
(=:) :: Type a => Ref a -> Data a -> Z inp out ()
v =: a = Z (v Ziria.=: a)

-- | Read a variable
readVar :: Type a => Ref a -> Z inp out (Data a)
readVar = Z . Ziria.readVar

-- | Emit a message to the output port
emit :: Data out -> Z inp out ()
emit = Z . Ziria.emit

-- | Receive a message from the input port
receiveVar :: Type inp => Ref inp -> Z inp out ()
receiveVar = Z . Ziria.receiveVar

-- | Receive a message from the input port
receive :: Type inp => Z inp out (Data inp)
receive = Z Ziria.receive

-- | Loop infinitely over the given program
loop :: Z inp out () -> Z inp out ()
loop = Z . Ziria.loop . unZ

