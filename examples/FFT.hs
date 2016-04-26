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
-- "Push-style" FFT
--------------------------------------------------------------------------------

fft :: Length
    -> Zun Samples Samples ()
fft n = fftCore n False >>> store >>> bitRev n

fftPar :: Length
       -> ParZun Samples Samples ()
fftPar n = fftCorePar n False |>>>| bitRev n

-- | Performs all 'ilog2 n' FFT/IFFT stages on a sample vector.
fftCore :: Length
        -> Bool  -- ^ Inverse?
        -> Zun Samples Samples ()
fftCore n inv = foldl1 (>>>) [ step inv $ value k | k <- Prelude.reverse [0..n'] ]
  where
    n' = floor (logBase 2 $ fromIntegral n) - 1

fftCorePar :: Length
        -> Bool  -- ^ Inverse?
        -> ParZun Samples Samples ()
fftCorePar n inv = foldl1 (|>>>|) [ liftP $ step inv $ value k | k <- Prelude.reverse [0..n'] ]
  where
    n' = floor (logBase 2 $ fromIntegral n) - 1

-- | Performs the 'k'th FFT/IFFT stage on a sample vector.
step :: Bool
     -> Data Length
     -> Zun Samples Samples ()
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
-- "Pull-style" FFT
--------------------------------------------------------------------------------

fft' n = (foldl1 (>>>) [ mkStage s | s <- [0..n'] ]) >>> bitRev 8
  where
    n' = floor (logBase 2 $ fromIntegral n) - 1
    mkStage :: Length -> Zun Samples Samples ()
    mkStage s =
        let tw = permute (\_ i -> i .<<. (i2n $ value s)) (twiddles 8)
        in  chunk (2 ^ s) (butterfly tw) >>> store

fftPar' n = (foldl1 (|>>>|) [ liftP $ mkStage s | s <- [0..n'] ]) |>>>| bitRev 8
  where
    n' = floor (logBase 2 $ fromIntegral n) - 1
    mkStage :: Length -> Zun Samples Samples ()
    mkStage s =
        let tw = permute (\_ i -> i .<<. (i2n $ value s)) (twiddles 8)
        in  chunk (2 ^ s) (butterfly tw)

-- | Twiddle factors for an `n`-point FFT.
twiddles :: Length -> Twiddles
twiddles n = Indexed (value n .>>. 1)
                     (\i -> polar 1 $ -π * 2 * i2n i / (i2n $ value n))

-- | A variable-width butterfly using the given twiddle factors.
butterfly :: Twiddles
          -> Samples
          -> Run Samples
butterfly twiddles samples = do
    let half = length samples .>>. 1
        (lower, upper) = splitAt half samples
    (left, right) <- force $ unzip $ zipWith dft2 twiddles $ zip lower upper
    return $ left +++ right

-- | 2-point Decimation-In-Frequency DFT
dft2 :: Num a => a -> (a, a) -> (a, a)
dft2 twiddle (a, b) = (a + b, twiddle * (a - b))

-- | Processing a vector in chunks.
chunk :: Length
      -> (Samples -> Run Samples)
      -> Zun Samples Samples ()
chunk chunks p = do
    v <- receive
    let len = length v `div` (value chunks)
    vs <- forM [0..chunks - 1] $ \c -> do
        let dropped = drop (value c * len) v
            items   = take len dropped
        lift $ p items
    emit $ concat vs

-- | Concatenation of multiple vectors (at least one).
concat :: Syntax a => [Vector a] -> Vector a
concat = foldl1 (+++)

-- | Vector concatenation.
(+++) :: Syntax a => Vector a -> Vector a -> Vector a
a +++ b = Indexed (aLen + bLen) $ \i -> (i < aLen) ? (a ! i) $ (b ! (i - aLen))
  where
    aLen = length a
    bLen = length b

--------------------------------------------------------------------------------
-- Testing utilities
--------------------------------------------------------------------------------

test :: Zun (Vector (Data (Complex Double))) (Vector (Data (Complex Double))) ()
     -> String -> Run ()
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

testPar :: ParZun (Vector (Data (Complex Double))) (Vector (Data (Complex Double))) ()
     -> String -> Run ()
testPar p inputFile = do
    translatePar
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
