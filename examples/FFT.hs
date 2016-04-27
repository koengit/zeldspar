module ZeldsparTest where

import Prelude hiding (div, concat, length, map, take, drop, zipWith, zip, unzip, splitAt, (/=), (<))

import Zeldspar
import Zeldspar.Parallel


type Samples  = Vector (Data (Complex Double))
type Twiddles = Vector (Data (Complex Double))


--------------------------------------------------------------------------------
-- Bit reversal
--------------------------------------------------------------------------------

-- | Rotates the 'k + 1' LSB bits right with 1.
rotBit :: Data Index -> Data Index -> Data Index
rotBit k i = lefts .|. rights
  where
    k'     = i2n k
    ir     = i .>>. 1
    rights = ir .&. oneBits k'
    lefts  = (((ir .>>. k') .<<. 1) .|. (i .&. 1)) .<<. k'

-- | Permute the vector by applying 'rotBit k' on its indices.
riffle :: Data Index -> Zun (Vector (Data a)) (Vector (Data a)) ()
riffle k = receive >>= emit . permute (const $ rotBit k)

-- | Bit reversal of a vector with length 'n'.
bitRev :: PrimType a => Length -> Zun (Vector (Data a)) (Vector (Data a)) ()
bitRev n = foldl1 (>>>) [ riffle $ value k | k <- [1..n'] ]
  where
    n' = floor (logBase 2 $ fromIntegral n) - 1

-- | Parallel bit reversal of a vector with length 'n'.
bitRevPar :: PrimType a => Length -> ParZun (Vector (Data a)) (Vector (Data a)) ()
bitRevPar n = foldl1 (|>>>|) [ liftP $ riffle $ value k | k <- [1..n'] ]
  where
    n' = floor (logBase 2 $ fromIntegral n) - 1


--------------------------------------------------------------------------------
-- "Pull-style" FFT
--------------------------------------------------------------------------------

fft :: Length -> Zun Samples Samples ()
fft n = fftCore n False >>> store >>> bitRev n

fftPar :: Length -> ParZun Samples Samples ()
fftPar n = fftCorePar n False |>>>| bitRevPar n

-- | Performs all 'ilog2 n' FFT/IFFT stages on a sample vector.
fftCore :: Length -> Bool -> Zun Samples Samples ()
fftCore n inv = foldl1 (>>>) [ step inv $ value k
                             | k <- Prelude.reverse [0..n'] ]
  where
    n' = floor (logBase 2 $ fromIntegral n) - 1

fftCorePar :: Length -> Bool -> ParZun Samples Samples ()
fftCorePar n inv = foldl1 (|>>>|) [ liftP $ step inv $ value k
                                  | k <- Prelude.reverse [0..n'] ]
  where
    n' = floor (logBase 2 $ fromIntegral n) - 1

-- | Performs the 'k'th FFT/IFFT stage on a sample vector.
step :: Bool -> Data Length -> Zun Samples Samples ()
step inv k = do
    v <- receive
    let ixf i = testBit i k ? (twid * (b - a)) $ (a + b)
          where
            k'   = i2n k
            a    = v ! i
            b    = v ! (i `xor` k2)
            twid = polar 1 ((if inv then π else -π) * i2n (lsbs k' i) / i2n k2)
            k2   = 1 .<<. k'
    emit $ Indexed (length v) ixf

-- | Indicates whether the 'i'th bit of 'a' is set.
testBit :: (Bits a, Num a, PrimType a) => Data a -> Data Index -> Data Bool
testBit a i = a .&. (1 .<<. i2n i) /= 0


--------------------------------------------------------------------------------
-- "Push-style" FFT
--------------------------------------------------------------------------------

fft' :: Length -> Zun Samples Samples ()
fft' n = (foldl1 (>>>) [ mkStage s | s <- [0..n'] ]) >>> bitRev n
  where
    n' = floor (logBase 2 $ fromIntegral n) - 1
    mkStage :: Length -> Zun Samples Samples ()
    mkStage s =
        let tw = permute (\_ i -> i .<<. (i2n $ value s)) (twiddles $ value n)
        in  chunk (2 ^ s) (butterfly tw) >>> store

fftPar' :: Length -> ParZun Samples Samples ()
fftPar' n = (foldl1 (|>>>|) [ liftP $ mkStage s | s <- [0..n'] ]) |>>>| bitRev n
  where
    n' = floor (logBase 2 $ fromIntegral n) - 1
    mkStage :: Length -> Zun Samples Samples ()
    mkStage s =
        let tw = permute (\_ i -> i .<<. (i2n $ value s)) (twiddles $ value n)
        in  chunk (2 ^ s) (butterfly tw)

-- | Twiddle factors for an `n`-point FFT.
twiddles :: Data Length -> Twiddles
twiddles n = Indexed (n .>>. 1) (\i -> polar 1 $ -π * 2 * i2n i / (i2n $ n))

-- | A variable-width butterfly using the given twiddle factors.
butterfly :: Twiddles -> Zun Samples Samples ()
butterfly twiddles = do
    samples <- receive
    let half = length samples .>>. 1
        (left, right) = unzip
                      $ zipWith dft2 twiddles
                      $ uncurry zip
                      $ splitAt half samples
    emit $ left +++ right

-- | 2-point Decimation-In-Frequency DFT.
dft2 :: Num a => a -> (a, a) -> (a, a)
dft2 twiddle (a, b) = (a + b, twiddle * (a - b))

-- | Vector concatenation.
(+++) :: Syntax a => Vector a -> Vector a -> Vector a
a +++ b = Indexed (aLen + bLen) $ \i -> (i < aLen) ? (a ! i) $ (b ! (i - aLen))
  where
    aLen = length a
    bLen = length b

-- | Processing vectors in chunks.
chunk :: Syntax a
      => Length
      -> Zun (Vector a) (Vector a) () -> Zun (Vector a) (Vector a) ()
chunk chunks p = do
    v <- receive
    let len = length v
    a <- lift $ newArr len
    let chunkSize = len `div` (value chunks)
        emits = forM_ [0..chunks - 1] $ \c -> do
            let dropped = drop (value c * chunkSize) v
                items   = take chunkSize dropped
            emit items
        receives = forM_ [0..chunks - 1] $ \c -> do
            items <- receive
            lift $ for (0, 1, Excl chunkSize) $ \i -> do
                setArr (value c * chunkSize + i) (items ! i) a
    emits >>> replicateM_ (fromIntegral chunks) p >>> receives
    ia <- lift $ unsafeFreezeArr a
    emit $ toPull $ Manifest len ia


--------------------------------------------------------------------------------
-- Testing utilities
--------------------------------------------------------------------------------

test :: Zun Samples Samples () -> String -> Run ()
test p inputFile = do
    translate
        p
        (do h <- fopen inputFile ReadMode
            n :: Data Length <- fget h
            input :: Arr (Complex Double) <- newArr n
            for (0, 1, Excl n) $ \i -> do
                re :: Data Double <- fget h
                im :: Data Double <- fget h
                setArr i (complex re im) input
            fclose h
            unsafeFreezeVec n input)
        (\output -> do
            let n = length output
            printf "%d\n" n
            for (0, 1, Excl n) $ \i -> do
                let xi :: Data (Complex Double) = output ! i
                    re = realPart xi
                    im = imagPart xi
                printf "%f %f\n" re im)

--------------------------------------------------------------------------------

testPar :: ParZun Samples Samples () -> String -> Run ()
testPar p inputFile = do
    translatePar 1024
        p
        (do h <- fopen inputFile ReadMode
            n :: Data Length <- fget h
            input :: Arr (Complex Double) <- newArr n
            for (0, 1, Excl n) $ \i -> do
                re :: Data Double <- fget h
                im :: Data Double <- fget h
                setArr i (complex re im) input
            fclose h
            inp <- unsafeFreezeVec n input
            return (inp, true))
        (\output -> do
            let n = length output
            printf "%d\n" n
            for (0, 1, Excl n) $ \i -> do
                let xi :: Data (Complex Double) = output ! i
                    re = realPart xi
                    im = imagPart xi
                printf "%f %f\n" re im
            return true)

--------------------------------------------------------------------------------

runLinked p = runCompiled' opts p
  where
    opts = defaultExtCompilerOpts
         { externalFlagsPost = ["-lpthread", "-lm"]
         , externalFlagsPre  = [ "-I../imperative-edsl/include"
                               , "../imperative-edsl/csrc/chan.c" ] }

{- expected results, for all a/b/c test file variants:

runLinked $ test (fft  8) "examples/FFT_in8a.txt" == "examples/FFT_out8a.txt"
runLinked $ test (fft' 8) "examples/FFT_in8a.txt" == "examples/FFT_out8a.txt"

runLinked $ testPar (fftPar  8) "examples/FFT_in8a.txt" == "examples/FFT_out8a.txt"
runLinked $ testPar (fftPar' 8) "examples/FFT_in8a.txt" == "examples/FFT_out8a.txt"

where '==' means that the files are the same, apart from numerical errors.

-}
