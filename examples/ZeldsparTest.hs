module ZeldsparTest where

import qualified Prelude

import Feldspar.Run
import Zeldspar
import Ziria


prog1 :: Zun (Data Int32) (Data Int32) ()
prog1 = {- loop $ -} do
    i <- receive
    lift $ printf "prog1 received %d\n" i
    emit (i+1)

prog2 :: Zun (Data Int32) (Data Int32) ()
prog2 = {- loop $ -} do
    i <- receive
    lift $ printf "prog2 received %d\n" i
    emit (i*2)

fused :: Zun (Data Int32) (Data Int32) ()
fused = prog1 >>> prog2

---

saving :: Zun (Data Int32) (Data Int32) ()
saving = do
    i <- receive
    r <- lift $ initRef i
    v <- lift $ getRef r
    lift $ printf "saved %d\n" v
    emit v

saved :: Zun (Data Int32) (Data Int32) ()
saved = prog1 >>> saving >>> prog2

---

prepare :: Zun (Data Int32) (Data Int32) () -> Run ()
prepare p = translate p src snk
  where
    src = fget stdin
    snk = printf "%d\n"

