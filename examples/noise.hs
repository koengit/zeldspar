{-# LANGUAGE TypeFamilies #-}
-- | Perlin noise implemented in Zeldspar.
--   TODO: PNG/BMP sink, to write pretty pictures to disk.
import qualified Prelude
import Zeldspar
import Zeldspar.Parallel

-- * Image settings; too large imgWidth, imgHeight or imgOctaves may cause you
--   to run out of stack space. Even when using small values, you should use
--   @ulimit@ to set a generous stack limit. Telling GCC to compile your program
--   with a larger stack also helps.
imgWidth = 400
imgHeight = 400
imgOctaves = 5
imgPersistence = 0.5

-- | Decently fast hash function.
xs32 :: Data Int32 -> Data Int32
xs32 x =
  share (x `xor` (x `shiftR` 13)) $ \x' ->
    share (x' `xor` (x' `shiftL` 17)) $ \x'' ->
      x'' `xor` (x'' `shiftR` 5)

-- | Hash over octave and coordinates.
xs32_2 :: Data Int32 -> Data Int32 -> Data Int32 -> Data Int32
xs32_2 octave x y =
  xs32 $ xs32 (octave*887) + xs32 (x*2251) + xs32 (y*7919)

-- | Ridiculous, inlined vector gradient function. Supposedly very fast,
--   but quite incomprehensible.
grad :: Data Int32
     -> Data Int32 -> Data Int32
     -> Data Double -> Data Double
     -> Data Double
grad octhash xi yi xf yf = share (xs32_2 octhash xi yi `rem` 8) $ \val ->
  (val == 0)
  ? (xf + yf)
  $ (val == 1)
    ? (yf - xf)
    $ (val == 2)
      ? (xf - yf)
      $ (val == 3)
        ? (negate xf - yf)
        $ (val == 4)
          ? xf
          $ (val == 5)
            ? negate xf
            $ (val == 6)
              ? yf
              $ negate yf

-- | Linear interpolation between two numbers.
lerp :: Data Double -> Data Double -> Data Double -> Data Double
lerp a b t = share a $ \a' -> a + t * (b-a)

-- | Smooth interpolation constant.
fade :: Data Double -> Data Double
fade t = share t $ \t' -> ((t' * t') * t') * (t' * (t' * 6 - 15) + 10)

-- | Basic implementation of Perlin noise for one octave: given a point in 2D
--   space, calculate the hash of each corner of the integral "box" surrounding
--   the point, e.g. if the point is (1.5, 1.5) the box is the described by
--   ((1, 1), (2, 2)).
--   Then, interpolate between these hashes depending on the point's location
--   inside the box. In the above example, the point (1.5, 1.5) is in the very
--   middle of the box ((1, 1), (2, 2)). The resulting value is the noise value
--   of that point.
--   Note that with Perlin noise, integral coordinates always have a zero noise
--   value.
perlin :: Data Int32 -> Data Double -> Data Double -> Data Double
perlin octave x y =
  share (xs32 octave) $ \octhash ->
    share (grad octhash x0i y0i (x-x0) (y-y0)) $ \a ->
      share (grad octhash x1i y0i (x-x1) (y-y0)) $ \b ->
        share (grad octhash x0i y1i (x-x0) (y-y1)) $ \c ->
          share (grad octhash x1i y1i (x-x1) (y-y1)) $ \d ->
            share (fade $ decimalPart x) $ \u ->
              share (fade $ decimalPart y) $ \v ->
                lerp (lerp a b u) (lerp c d u) v
  where
    x0i = f2n x0
    y0i = f2n y0
    x1i = f2n x1
    y1i = f2n y1
    x0 = floor x
    y0 = floor y
    x1 = x0 + 1
    y1 = y0 + 1

decimalPart :: Data Double -> Data Double
decimalPart x = x - floor x

-- TODO: only works for positive numbers
f2n :: Data Double -> Data Int32
f2n x = share (round x) $ \xi ->
  x - i2n xi >= 0 ? xi $ xi-1

-- TODO: only works for positive numbers
floor :: Data Double -> Data Double
floor = i2n . f2n

pow :: Data Double -> Word32 -> Data Double
pow n 1 = n
pow n k = pow (n*n) (k-1)

-- | Use one Zeldspar pipeline stage for each noise octave.
--   Replace @|>> 5 `ofLength` sz >>|@ with @>>>@ for the sequential version.
perlinVec :: Data Word32
          -> Data Word32
          -> Int32
          -> Data Double
          -> ParZun (Manifest (Data Double)) (Manifest (Data Double)) ()
perlinVec width height octaves persistence = liftP $ do
    go 1 1 1
  where
    maxdensity _ 1 _ = 1
    maxdensity p o a = maxdensity p (o-1) (a*p) + a*p
    sz = width*height
    
    go octave freq amp
      | octave Prelude.>= octaves =
        step (Prelude.fromIntegral octave) freq amp
          |>> 5 `ofLength` sz >>| finalize
      | otherwise =
        step (Prelude.fromIntegral octave) freq amp
          |>> 5 `ofLength` sz >>| go (octave+1) (freq*2) (amp*persistence)

    -- Divide all noise values by the maximum possible noise value.
    finalize = loop $ do
      let md = maxdensity persistence octaves 1
      inp <- take
      out <- lift $ manifestFresh $ map (/md) inp
      emit out

    -- Calculate noise value for one octave.
    -- TODO: get rid of the manifest vector of coordinates.
    step octave freq amp = loop $ do
      inp <- take
      let xs = replicate height $ (0 ... width)
          ys = map (replicate width) $ (0 ... height)
          coords = concat height $ zipWith zip xs ys
      coords' <- lift $ manifestFresh coords
      m <- lift $ manifestFresh $ map addOctave (zip coords' inp)
      emit m
      where
        addOctave :: ((Data Index, Data Index), Data Double) -> Data Double
        addOctave ((x, y), total) =
          total + amp*perlin octave (i2n x/75*freq) (i2n y/75*freq)

-- | Add a source and a sink to the program, producing ten identical images.
--   The sink prints the noise values ad some arbitrarily selected coordinates.
preparePar :: Data Length
           -> Data Length
           -> ParZun (Manifest (Data Double)) (Manifest (Data Double)) ()
           -> Run ()
preparePar w h p = do
    r <- initRef 0
    runParZ p (src r) (5 `ofLength` sz) snk (5 `ofLength` sz)
  where
    sz = w*h

    src :: Ref (Data Int32) -> Run (Manifest (Data Double), Data Bool)
    src r = do
      count <- getRef r
      setRef r (count+1)
      m <- manifestFresh $ replicate (h*w) (0 :: Data Double)
      pure (m, count < 10)

    snk :: Manifest (Data Double) -> Run (Data Bool)
    snk v = do
      printf "%f %f %f %f %f\n" (v ! 801) (v ! 802) (v ! 803) (v ! 804) (v ! 805)
      pure true

-- | Compile the program to a C file.
--   The file needs to be compiled with
--   @-lm -lpthread -I/path_to_imperative-edsl/include path_to_imperative-edsl/csrc/chan.c@.
compileToC :: IO ()
compileToC =
  Prelude.writeFile "noise.c"
  $ compile
  $ preparePar imgWidth imgHeight
  $ perlinVec imgWidth imgHeight imgOctaves imgPersistence

main = compileToC
