module ZeldsparTest where



import Prelude ()

import Feldspar hiding (getRef, modifyRef)
import Feldspar.SimpleVector
import Feldspar.IO hiding (compile)

import Zeldspar



kernel :: Data Int32 -> Data Int32
kernel n = sum $ map (\x -> x*x) (0...n)

prog1 :: Z Int32 Int32 ()
prog1 = loop $ do
    i <- receive
    emit $ kernel i

prog2 :: Z Int32 Int32 ()
prog2 = loop $ do
    i <- receive
    j <- receive
    emit (i*j*3333)

delay = do
    ir <- initRef (0 :: Data Word32)
    while (fmap (<100000) $ getRef ir) $ modifyRef ir (+1)

prog3 = do
    inp <- fopen "input" ReadMode
    out <- fopen "output" WriteMode
    printf "files open, starting to loop..."
    translate (prog1 >>> prog2) (fget' inp) (fprintf out "%d ")
  where
    fget' h = do
        delay  -- The program is an infinite loop so it has to be slowed down.
               -- Otherwise it will quickly produce a gigantic file.
        end <- feof h
        return 0
        ifE end (return 0) (fget h)

test1   = compile prog1
test1_2 = compile (prog1 >>> prog2)
test3   = Feldspar.IO.icompile prog3

