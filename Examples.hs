module Examples where



import Frontend
import RunIO



source :: IO Int
source = fmap (read . return) getChar

sink :: Int -> IO ()
sink i = putStr (show i) >> putStr " "

run :: Prog Exp Int Int () -> IO ()
run p = runIO (runProg p) source sink

prog1 :: Prog Exp Int Int ()
prog1 = loop $ do
    r <- newRef
    receive r
    i <- getRef r
    emit (i+1)

prog2 :: Prog Exp Int Int ()
prog2 = loop $ do
    r <- newRef
    receive r
    i <- getRef r
    emit (i*2)

prog3 = prog1 >>> prog2

