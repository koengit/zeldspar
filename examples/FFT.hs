module ZeldsparTest where

import Prelude hiding (div, concat, length, map, take, drop, zipWith, zip, unzip, splitAt, (/=), (<))

import Zeldspar
import Zeldspar.Parallel


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

-- | Indicates whether the 'i'th bit of 'a' is set.
testBit :: (Bits a, Num a, PrimType a) => Data a -> Data Index -> Data Bool
testBit a i = a .&. (1 .<<. i2n i) /= 0

-- | Performs the 'k'th FFT/IFFT stage on a sample vector.
step :: Bool
     -> Data Length
     -> Zun (Vector (Data (Complex Double))) (Vector (Data (Complex Double))) ()
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

-- | Performs all 'ilog2 n' FFT/IFFT stages on a sample vector.
fftCore :: Length
        -> Bool  -- ^ Inverse?
        -> Zun (Vector (Data (Complex Double))) (Vector (Data (Complex Double))) ()
fftCore n inv = foldl1 (>>>) [ step inv $ value k | k <- Prelude.reverse [0..n'] ]
  where
    n' = floor (logBase 2 $ fromIntegral n) - 1

fftCorePar :: Length
        -> Bool  -- ^ Inverse?
        -> ParZun (Vector (Data (Complex Double))) (Vector (Data (Complex Double))) ()
fftCorePar n inv = foldl1 (|>>>|) [ liftP $ step inv $ value k | k <- Prelude.reverse [0..n'] ]
  where
    n' = floor (logBase 2 $ fromIntegral n) - 1

--------------------------------------------------------------------------------

fft :: Length
    -> Zun (Vector (Data (Complex Double))) (Vector (Data (Complex Double))) ()
fft n = fftCore n False >>> store >>> bitRev n

fftPar :: Length
       -> ParZun (Vector (Data (Complex Double))) (Vector (Data (Complex Double))) ()
fftPar n = fftCorePar n False |>>>| bitRev n

--------------------------------------------------------------------------------

-- Work in progress ideas / drafts

fft8 = do
    v <- receive
    tw8 <- lift $ force $ twiddles 8
    let evens = permute $ \_ i -> i .<<. 1
        tw4 = evens tw8
        tw2 = evens tw4
        stage0 = chunk 1 (butterfly tw8)
        stage1 = chunk 2 (butterfly tw4)
        stage2 = chunk 4 (butterfly tw2)
    stage0 >>> store >>> stage1 >>> store >>> stage2 >>> store >>> bitRev 8

-- | Twiddle factors for an `n`-point FFT.
twiddles :: Length -> Vector (Data (Complex Double))
twiddles n = Indexed (value n .>>. 1)
                     (\i -> polar 1 $ -π * 2 * i2n i / (i2n $ value n))

-- | A variable-width butterfly using the given twiddle factors.
butterfly :: Vector (Data (Complex Double))
          -> Vector (Data (Complex Double))
          -> Run (Vector (Data (Complex Double)))
butterfly twiddles v = do
    let half = length v .>>. 1
        (lower, upper) = splitAt half v
    (left, right) <- force $ unzip $ zipWith dft2 twiddles $ zip lower upper
    return $ left +++ right

-- | 2-point Decimation-In-Frequency DFT
dft2 :: Num a => a -> (a, a) -> (a, a)
dft2 twiddle (a, b) = (a + b, twiddle * (a - b))

-- | Processing a vector in chunks.
chunk :: Length
      -> (Vector (Data (Complex Double))
          -> Run (Vector (Data (Complex Double))))
      -> Zun (Vector (Data (Complex Double))) (Vector (Data (Complex Double))) ()
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

{-
halve :: Type a => Zun (Vector (Data a)) (Vector (Data a)) ()
halve = do
    v <- receive
    let n2 = length v .>>. 1
    emit $ take n2 v
    emit $ drop n2 v

unhalve :: Type a => Zun (Vector (Data a)) (Vector (Data a)) ()
unhalve = do
    v1 <- receive
    v2 <- receive
    emit $ v1 +++ v2

-- | Two-point DFT butterfly for Decimation-In-Frequency
dft2 :: Data (Complex Double)
     -> Zun (Vector (Data (Complex Double))) (Vector(Data (Complex Double))) ()
dft2 twiddle = do
    v <- receive
    let a = v ! 0
        b = v ! 1
        v' = Indexed 2 f
        f i = i /= 1 ? (twiddle * (b - a)) $ (a + b)
    lift $ printf "dft2 (twiddle %f %f) (A: %f %f) (B: %f %f)\n"
        (realPart twiddle) (imagPart twiddle)
        (realPart a) (imagPart a)
        (realPart b) (imagPart b)
    emit v'
-}

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
