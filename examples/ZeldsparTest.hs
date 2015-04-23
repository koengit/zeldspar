module ZeldsparTest where



import qualified Prelude

import Feldspar
import Feldspar.SimpleVector

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

test1_1 = compile prog1
test1_2 = compile (prog1 >>> prog2)

