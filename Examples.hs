module Examples where



import Language.Embedded.Expr

import Frontend
import RunIO



source :: IO Int
source = fmap (read . return) getChar

sink :: Int -> IO ()
sink i = putStr (show i) >> putStr " "

run :: Prog Expr Int Int a -> IO a
run p = runIO p source sink

prog1 :: Prog Expr Int Int ()
prog1 = do
    r <- newRef
    loop $ do
      receive r
      i <- getRef r
      emit (i+1)

prog2 :: Prog Expr Int Int ()
prog2 = do
    r <- newRef
    loop $ do
      receive r
      i <- getRef r
      emit (i*2)

prog12 = prog1 >>> prog2

prog3 :: Prog Expr Int Int ()
prog3 = do
    r <- newRef
    loop $ do
      receive r
      i <- getRef r
      emit (i+1)
      emit (i+2)

prog4 :: Prog Expr Int Int ()
prog4 = do
    r <- newRef
    loop $ do
      receive r
      i <- getRef r
      emit (i*2)
      receive r
      i <- getRef r
      emit (i*3)
      receive r
      i <- getRef r
      emit (i*4)

prog34 = prog3 >>> prog4

