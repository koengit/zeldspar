{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

module Zeldspar where



import Control.Applicative (Applicative)

import Language.Embedded.Imperative

import Feldspar hiding (Ref)
import Feldspar.Compiler.FromImperative ()

import qualified Ziria



newtype Z inp out a = Z { unZ :: Ziria.Z Data inp out a }
  deriving (Functor, Applicative, Monad)

runZeld
    :: Z inp out a
    -> IO inp          -- ^ Source
    -> (out -> IO ())  -- ^ Sink
    -> IO a
runZeld = Ziria.runIO . unZ

compile
    :: ( RefCMD Data     :<: instr
       , ControlCMD Data :<: instr
       , IExp instr ~ Data
       )
    => Z inp out a
    -> Program instr (Data inp)        -- ^ Source
    -> (Data out -> Program instr ())  -- ^ Sink
    -> Program instr a
compile = Ziria.compile . unZ

icompile :: Z Float Float a -> IO ()
icompile = Ziria.icompile . unZ

(>>>) :: Z inp msg () -> Z msg out () -> Z inp out ()
Z p1 >>> Z p2 = Z (p1 Ziria.>>> p2)

newVar :: Type a => Z inp out (Ref a)
newVar = Z Ziria.newVar

initVar :: Type a => Data a -> Z inp out (Ref a)
initVar = Z . Ziria.initVar

(=:) :: Type a => Ref a -> Data a -> Z inp out ()
v =: a = Z (v Ziria.=: a)

readVar :: Type a => Ref a -> Z inp out (Data a)
readVar = Z . Ziria.readVar

emit :: Data out -> Z inp out ()
emit = Z . Ziria.emit

receiveVar :: Type inp => Ref inp -> Z inp out ()
receiveVar = Z . Ziria.receiveVar

receive :: Type inp => Z inp out (Data inp)
receive = Z Ziria.receive

loop :: Z inp out () -> Z inp out ()
loop = Z . Ziria.loop . unZ

