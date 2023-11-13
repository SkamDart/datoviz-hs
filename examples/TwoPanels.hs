{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NumericUnderscores #-}
module Main where

import Data.Word
import Graphics.Datoviz
import qualified Linear.V3 as L
import qualified Data.Vector.Storable as VS

foreign import capi unsafe "datoviz/datoviz.h" dvz_rand_normal :: IO Float
foreign import capi unsafe "datoviz/datoviz.h" dvz_rand_float :: IO Float

main :: IO ()
main = withDvzApp DVZ_BACKEND_GLFW $ \app -> do
  gpu <- dvz_gpu_best app
  canvas <- dvz_canvas gpu 2_560 1_440 0
  dvz_canvas_clear_color canvas 1 1 1
  scene <- dvz_scene canvas 1 2
  axesPanel <- dvz_scene_panel scene 0 0 DVZ_CONTROLLER_AXES_2D 0
  arcballPanel <- dvz_scene_panel scene 0 1 DVZ_CONTROLLER_ARCBALL 0
  axesVisual <- dvz_scene_visual axesPanel DVZ_VISUAL_POINT 0
  arcballVisual <- dvz_scene_visual arcballPanel DVZ_VISUAL_POINT 0
  let n = 50_000 :: Word32
  pos <- mkPosVec $ fromIntegral n
  color <- mkColorVec $ fromIntegral n

  dvz_visual_data axesVisual DVZ_PROP_POS 0 n pos
  dvz_visual_data axesVisual DVZ_PROP_COLOR 0 n color

  dvz_visual_data arcballVisual DVZ_PROP_POS 0 n pos
  dvz_visual_data arcballVisual DVZ_PROP_COLOR 0 n pos

  dvz_app_run app 0

mkPosVec :: Int -> IO (VS.Vector DVec3)
mkPosVec n = VS.generateM n go
    where
        go _ = do
            z <- dvz_rand_normal
            o <- dvz_rand_normal
            pure $ DVec3 (fmap realToFrac (L.V3 z o 0.0))

mkColorVec :: Int -> IO (VS.Vector CVec4)
mkColorVec n = VS.generateM n go
    where
        go _ = do
            c <- dvz_rand_float
            dvz_colormap_scale DVZ_CMAP_VIRIDIS (realToFrac c) 0 1


