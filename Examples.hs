module Examples where



import Language.Embedded.Expr

import Zeldspar



sourceIO :: IO Float
sourceIO = fmap (read . return) getChar

sinkIO :: Float -> IO ()
sinkIO i = putStr (show i) >> putStr " "

run :: Z Expr Float Float a -> IO a
run p = runIO p sourceIO sinkIO



----------------------------------------------------------------------------------------------------
-- * Examples
----------------------------------------------------------------------------------------------------

prog1 :: Z Expr Float Float ()
prog1 = do
    r <- newVar
    loop $ do
      receive r
      i <- readVar r
      emit (i+1)

prog2 :: Z Expr Float Float ()
prog2 = do
    r <- newVar
    loop $ do
      receive r
      i <- readVar r
      emit (i*2)

run12  = run      (prog1 >>> prog2)
comp12 = icompile (prog1 >>> prog2)

prog3 :: Z Expr Float Float ()
prog3 = do
    r <- newVar
    loop $ do
      receive r
      i <- readVar r
      emit (i+1)
      emit (i+2)

prog4 :: Z Expr Float Float ()
prog4 = do
    r <- newVar
    loop $ do
      receive r
      i <- readVar r
      emit (i*2)
      receive r
      i <- readVar r
      emit (i*3)
      receive r
      i <- readVar r
      emit (i*4)

run34  = run      (prog3 >>> prog4)
comp34 = icompile (prog3 >>> prog4)

