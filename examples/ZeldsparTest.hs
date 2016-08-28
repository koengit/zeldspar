module ZeldsparTest where

import qualified Prelude

import Zeldspar
import Zeldspar.Parallel


prog1 :: Zun (Data Int32) (Data Int32) ()
prog1 = loop $ do
    i <- take
    lift $ printf "prog1 received %d\n" i
    emit (i + 1)

prog2 :: Zun (Data Int32) (Data Int32) ()
prog2 = loop $ do
    i <- take
    lift $ printf "prog2 received %d\n" i
    emit (i * 2)

fused :: Zun (Data Int32) (Data Int32) ()
fused = prog1 >>> prog2

stored :: Zun (Data Int32) (Data Int32) ()
stored = prog1 >>> loop store >>> prog2

---

prog3 :: Zun (Data Int32) (Data Int32) ()
prog3 = loop $ do
    i <- take
    emit (i + 1)
    emit (i + 2)

prog4 :: Zun (Data Int32) (Data Int32) ()
prog4 = loop $ do
    i <- take
    emit (i * 2)
    i <- take
    emit (i * 3)
    i <- take
    emit (i * 4)

infinite :: Zun (Data Int32) (Data Int32) ()
infinite = prog3 >>> prog4

---

infinite' :: Zun (Data Int32) (Data Int32) ()
infinite' = (emit 13 >> prog3) >>> prog4

---

vecMake :: Zun (Data Int32) (DPull Int32) ()
vecMake = loop $ do
    i <- take
    emit $ fmap (i2n) (0 ... i2n i)

vecInc :: (PrimType a, Num a) => Zun (DPull a) (DPull a) ()
vecInc = loop $ do
    v <- take
    emit (fmap (+1) v)

vecRev :: PrimType a => Zun (DPull a) (DPull a) ()
vecRev = loop $ do
    v <- take
    emit (reverse v)

vecTail :: Zun (DPull Int32) (DPull Int32) ()
vecTail = loop $ do
    v <- take
    lift $ printf "Dropping: %d\n" (head v)
    emit (drop 1 v)

vecSum :: Zun (DPull Int32) (Data Int32) ()
vecSum = loop $ do
    v <- take
    emit (sum v)

fusedVec = vecMake >>> vecInc >>> vecRev >>> vecTail >>> vecSum

storedVec = vecMake >>> vecInc >>> loop store >>> vecRev >>> vecTail >>> vecSum

parVec = (vecMake >>> vecInc) |>>10`ofLength`30>>| (vecRev >>> (vecTail >>> vecSum))

parVec' n = vecMake |>>4`ofLength`n>>| vecTail |>>3`ofLength`n>>| vecSum |>>2>>| prog1

---

prepare :: Zun (Data Int32) (Data Int32) () -> Run ()
prepare p = runZ p src snk
  where
    src = fget stdin
    snk = printf "%d\n"

---

preparePar :: ParZun (Data Int32) (Data Int32) () -> Run ()
preparePar p = runParZ p src 10 snk 10
  where
    src = (\x -> (x, true)) <$> fget stdin
    snk = \x -> printf "%d\n" x >> return true

runPar p = runCompiled' def opts (preparePar p)
  where
    opts = def
         { externalFlagsPost = ["-lpthread"]
         , externalFlagsPre  = [ "-I../imperative-edsl/include"
                               , "../imperative-edsl/csrc/chan.c" ] }
