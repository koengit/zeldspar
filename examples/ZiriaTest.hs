module ZiriaTest where



import Language.Embedded.Expr

import Ziria
import Parallel

sourceIO :: IO Float
sourceIO = fmap (read . return) getChar

sinkIO :: Float -> IO ()
sinkIO i = putStr (show i) >> putStr " "



prog1 :: Z Expr Float Float ()
prog1 = loop $ do
    i <- receive
    emit (i+1)

prog2 :: Z Expr Float Float ()
prog2 = loop $ do
    i <- receive
    emit (i*2)

run12  = runIO    (prog1 >>> prog2) sourceIO sinkIO
comp12 = icompile (prog1 >>> prog2)

run12par  = runPar      (prog1 |>>>| prog2) sourceIO sinkIO
comp12par = icompilePar (prog1 |>>>| prog2)

prog3 :: Z Expr Float Float ()
prog3 = loop $ do
    i <- receive
    emit (i+1)
    emit (i+2)

prog4 :: Z Expr Float Float ()
prog4 = loop $ do
    i <- receive
    emit (i*2)
    i <- receive
    emit (i*3)
    i <- receive
    emit (i*4)

run34  = runIO   (prog3 >>> prog4) sourceIO sinkIO
comp34 = compile (prog3 >>> prog4)
