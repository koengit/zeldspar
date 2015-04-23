{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- | An implementation of Ziria that uses Feldspar to represent pure computations
module Zeldspar where



import Control.Applicative (Applicative)

import Language.Embedded.Imperative

import Feldspar hiding (Ref)
import Feldspar.Compiler.FromImperative ()

import qualified Ziria



-- | The type of sequential Zeldspar programs
newtype Z inp out a = Z { unZ :: Ziria.Z Data inp out a }
  deriving (Functor, Applicative, Monad)

-- | Interpret a Zeldspar program in the 'IO' monad
runZeld
    :: Z inp out a
    -> IO inp          -- ^ Source
    -> (out -> IO ())  -- ^ Sink
    -> IO a
runZeld = Ziria.runIO . unZ

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

-- | Simplified compilation from 'Z' to C. Input/output is done via two external functions: @source@
-- and @sink@.
compileStr :: Type inp => Z inp out a -> String
compileStr = Ziria.compileStr . unZ

-- | Simplified compilation from 'Z' to C. Input/output is done via two external functions: @source@
-- and @sink@.
compile :: Type inp => Z inp out a -> IO ()
compile = Ziria.compile . unZ

-- | Program composition. The programs are always fused.
(>>>) :: Z inp msg () -> Z msg out () -> Z inp out ()
Z p1 >>> Z p2 = Z (p1 Ziria.>>> p2)

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

