module ZeldsparTest where

import qualified Prelude

import Zeldspar
import Zeldspar.Parallel


prog1 :: Zun (Data Int32) (Data Int32) ()
prog1 = loop $ do
    i <- receive
    lift $ printf "prog1 received %d\n" i
    emit (i + 1)

prog2 :: Zun (Data Int32) (Data Int32) ()
prog2 = loop $ do
    i <- receive
    lift $ printf "prog2 received %d\n" i
    emit (i * 2)

fused :: Zun (Data Int32) (Data Int32) ()
fused = prog1 >>> prog2

stored :: Zun (Data Int32) (Data Int32) ()
stored = prog1 >>> store >>> prog2

---

prog3 :: Zun (Data Int32) (Data Int32) ()
prog3 = loop $ do
    i <- receive
    emit (i + 1)
    emit (i + 2)

prog4 :: Zun (Data Int32) (Data Int32) ()
prog4 = loop $ do
    i <- receive
    emit (i * 2)
    i <- receive
    emit (i * 3)
    i <- receive
    emit (i * 4)

infinite :: Zun (Data Int32) (Data Int32) ()
infinite = prog3 >>> prog4

---

infinite' :: Zun (Data Int32) (Data Int32) ()
infinite' = (emit 13 >> prog3) >>> prog4

---

vecMake :: Zun (Data Int32) (Vector (Data Int32)) ()
vecMake = loop $ do
    i <- receive
    emit $ map (i2n) (0 ... i2n i)

vecInc :: (PrimType a, Num a) => Zun (Vector (Data a)) (Vector (Data a)) ()
vecInc = loop $ do
    v <- receive
    emit (map (+1) v)

vecRev :: PrimType a => Zun (Vector (Data a)) (Vector (Data a)) ()
vecRev = loop $ do
    v <- receive
    emit (reverse v)

vecTail :: Zun (Vector (Data Int32)) (Vector (Data Int32)) ()
vecTail = loop $ do
    v <- receive
    lift $ printf "Dropping: %d\n" (head v)
    emit (drop 1 v)

vecSum :: Zun (Vector (Data Int32)) (Data Int32) ()
vecSum = loop $ do
    v <- receive
    emit (sum v)

fusedVec = vecMake >>> vecInc >>> vecRev >>> vecTail >>> vecSum

storedVec = vecMake >>> vecInc >>> store >>> vecRev >>> vecTail >>> vecSum

parVec = (vecMake >>> vecInc) |>>>| (vecRev >>> (vecTail >>> vecSum))

parVecSized = (vecMake >>> vecInc) |>> 64 >>| (vecRev >>> (vecTail >>> vecSum))

parVecSized' = vecMake |>> 17 >>| vecTail |>> 16 >>| vecSum |>> 2 >>| prog1

---

prepare :: Zun (Data Int32) (Data Int32) () -> Run ()
prepare p = translate p src snk
  where
    src = fget stdin
    snk = printf "%d\n"

---

preparePar :: ParZun (Data Int32) (Data Int32) () -> Run ()
preparePar p = translatePar 1024 p src snk
  where
    src = (\x -> (x, true)) <$> fget stdin
    snk = \x -> printf "%d\n" x >> return true

runPar p = runCompiled' opts (preparePar p)
  where
    opts = defaultExtCompilerOpts
         { externalFlagsPost = ["-lpthread"]
         , externalFlagsPre  = [ "-I../imperative-edsl/include"
                               , "../imperative-edsl/csrc/chan.c" ] }
