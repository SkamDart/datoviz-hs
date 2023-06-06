{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as BL
import Data.Csv
import Data.Csv.Incremental as I
import Data.Traversable
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as U
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Graphics.Datoviz
import Linear
import Options.Applicative as O
import System.IO

words' :: Char -> String -> [String]
words' c s =
  case dropWhile (== c) s of
    [] -> []
    s' -> w : words' c s'' where (w, s'') = break (== c) s'

data Config = Config {xcol :: Int, ycols :: [Int], filePath :: FilePath}
  deriving (Eq, Ord, Show)

parseFilePath :: O.Parser FilePath
parseFilePath = argument str (metavar "FILE" <> help "The CSV file to obtain the data from.")

parseXCol :: O.Parser Int
parseXCol =
  option auto $
    mconcat
      [ long "independent-variable",
        short 'x',
        help "The column of the CSV file to use as the x-axis.",
        value 0,
        metavar "COL_NUM"
      ]

parseYCols :: O.Parser [Int]
parseYCols =
  option col $
    mconcat
      [ long "dependent-variables",
        short 'y',
        help "The columns of the CSV file to use as the heights of each line. Comma-separated sequence of ints.",
        value [1],
        metavar "COL_NUMS"
      ]
  where
    col = eitherReader $ \ys -> for (words' ',' ys) $ \s -> case reads s of
      [(r, [])] -> pure r
      _ -> Left $ "cannot parse value `" <> s <> "'"

parseConfig :: O.Parser Config
parseConfig = Config <$> parseXCol <*> parseYCols <*> parseFilePath

parserOpts :: O.ParserInfo Config
parserOpts =
  info
    (parseConfig <**> helper)
    (fullDesc <> progDesc "Open a window with an interactive line plot of some CSV columns.")

main :: IO ()
main = do
  conf <- execParser parserOpts
  -- We create a singleton application with the GLFW backend.
  withDvzApp DVZ_BACKEND_GLFW $ \app -> do
    -- Get the best GPU for the job.
    gpu <- dvz_gpu_best app

    -- Create a canvas with a specified size.
    canvas <- dvz_canvas gpu 2560 1440 0x03 -- 0x03 => show FPS

    -- We use a white background color (RGB floating-point values in [0, 1]).
    dvz_canvas_clear_color canvas 1 1 1

    -- We create a scene, which allows us to define several subplots (panels) organized within a
    -- grid. Here, we just use a single panel spanning the entire canvas.
    scene <- dvz_scene canvas 1 1

    -- We get the panel at row 0, column 0, and we initialize it with an axes 2D controller.
    -- The last argument is for optional flags.
    panel <- dvz_scene_panel scene 0 0 DVZ_CONTROLLER_AXES_2D 0

    -- visual data but we don't do for-loops here
    withLoadedCSV (filePath conf) (xcol conf) (ycols conf) $ \arrs -> do
      k <- peek $ dynArraySize (arrs V.! 0)
      V.forM_ (V.zip (V.enumFromN 0 k :: V.Vector Int) arrs) $ \(i, arr) -> do
        visual <- dvz_scene_visual panel DVZ_VISUAL_LINE_STRIP 0
        col <-
          VS.replicateM k $
            dvz_colormap_scale
              DVZ_CMAP_VIRIDIS
              (fromIntegral (i + 1) / fromIntegral (length (ycols conf) + 1))
              0
              1
        ptr <- peek (dynArrayPtr arr)
        n <- peek (dynArraySize arr)
        c_dvz_visual_data visual DVZ_PROP_POS 0 (fromIntegral n) (castPtr ptr)
        dvz_visual_data visual DVZ_PROP_COLOR 0 (fromIntegral n) col
      dvz_app_run app 0

data DynArray a = DynArray
  { dynArrayPtr :: {-# UNPACK #-} !(Ptr (Ptr a)),
    dynArraySize :: {-# UNPACK #-} !(Ptr Int),
    dynArrayCapacity :: {-# UNPACK #-} !(Ptr Int)
  }

withDynArray :: forall a b. Storable a => (DynArray a -> IO b) -> IO b
withDynArray f = alloca $ \ptrptr -> alloca $ \sz -> alloca $ \cap -> do
  let n = 16
  poke cap n
  poke sz 0
  bracket
    (mallocBytes (n * sizeOf (undefined :: a)) >>= poke ptrptr . castPtr)
    (\_ -> peek ptrptr >>= free)
    $ \_ -> f $ DynArray ptrptr sz cap

withDynArrays :: Storable a => Int -> (V.Vector (DynArray a) -> IO b) -> IO b
withDynArrays n f = go n []
  where
    go 0 xs = f (V.fromList xs)
    go k xs = withDynArray $ \x -> go (k - 1) (x : xs)

appendDynArray :: forall a. Storable a => a -> DynArray a -> IO ()
appendDynArray a arr = do
  cap <- peek (dynArrayCapacity arr)
  sz <- peek (dynArraySize arr)
  ptr <- peek (dynArrayPtr arr)
  ptr' <-
    if sz == cap
      then
        let cap' = (2 * cap * sizeOf (undefined :: a))
         in poke (dynArrayCapacity arr) (2 * cap) >> reallocBytes ptr cap'
      else pure ptr
  poke (dynArrayPtr arr) ptr'
  pokeElemOff ptr' sz a
  poke (dynArraySize arr) (sz + 1)

withLoadedCSV :: FilePath -> Int -> [Int] -> (V.Vector (DynArray DVec3) -> IO b) -> IO b
withLoadedCSV file x ys f = withFile file ReadMode $ \h -> withDynArrays (length ys) $ \arrs -> do
  loop arrs (I.decode HasHeader) h
  f arrs
  where
    loop _ (Fail _ errMsg) _ = error errMsg
    loop arrs (Many !rs k) h = do
      forM_ rs $ \case
        Right a -> forM_ (zip [0 ..] ys) $ \(i, y) ->
          appendDynArray (DVec3 $ V3 (a U.! x) (a U.! y) 0) (arrs V.! i)
        Left msg -> error msg
      feed k h >>= \p -> loop arrs p h
    loop arrs (Done rs) _ = do
      forM_ rs $ \case
        Right a -> forM_ (zip [0 ..] ys) $ \(i, y) ->
          appendDynArray (DVec3 $ V3 (a U.! x) (a U.! y) 0) (arrs V.! i)
        Left msg -> error msg
    feed k h = do
      isEof <- hIsEOF h
      if isEof then pure $ k BL.empty else k <$> BL.hGetSome h 4096
