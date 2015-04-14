{-# LANGUAGE NoMonomorphismRestriction #-}

module Examples where



import Language.C.Monad
import Language.Embedded.Backend.C ()
import Language.Embedded.Imperative (interpret, printf)
import Language.Embedded.Expr

import Frontend
import RunIO
import Backend



sourceIO :: IO Int
sourceIO = fmap (read . return) getChar

sinkIO :: Int -> IO ()
sinkIO i = putStr (show i) >> putStr " "

run :: Prog Expr Int Int a -> IO a
run p = runIO p sourceIO sinkIO

sourceComp = return 4
sinkComp   = printf " <%d> "

comp prog = prettyCGen $ wrapMain $ interpret $ compile prog sourceComp sinkComp



----------------------------------------------------------------------------------------------------
-- * Examples
----------------------------------------------------------------------------------------------------

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

run12  = run  (prog1 >>> prog2)
comp12 = comp (prog1 >>> prog2)

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

run34  = run  (prog3 >>> prog4)
comp34 = comp (prog3 >>> prog4)

