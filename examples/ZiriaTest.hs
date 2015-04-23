module ZiriaTest where



import Language.Embedded.Expr

import Ziria
import Parallel

sourceIO :: IO Float
sourceIO = fmap (read . return) getChar

sinkIO :: Float -> IO ()
sinkIO i = putStr (show i) >> putStr " "

run :: Z Expr Float Float a -> IO a
run p = runIO p sourceIO sinkIO



prog1 :: Z Expr Float Float ()
prog1 = loop $ do
    i <- receive
    emit (i+1)

prog2 :: Z Expr Float Float ()
prog2 = loop $ do
    i <- receive
    emit (i*2)

run12  = run     (prog1 >>> prog2)
comp12 = compile (prog1 >>> prog2)

run12par  = runPar     (prog1 |>>>| prog2) sourceIO sinkIO
comp12par = compilePar (prog1 |>>>| prog2)

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

run34  = run     (prog3 >>> prog4)
comp34 = compile (prog3 >>> prog4)

