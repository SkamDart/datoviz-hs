{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE NumericUnderscores #-}
module Main where

import Control.Concurrent
import Graphics.Datoviz
import qualified Linear.V3 as L
import qualified Data.Vector.Storable as VS
import qualified Numeric.LinearAlgebra.Data as HD
import qualified Numeric.LinearAlgebra as H

-- Parameters
-- ----------
-- Number of signals to display.
nSignals :: Int
nSignals = 1

-- Numer of points per signal.
nPoints :: Int
nPoints = 1_000

-- Total number of vertices.
nVert :: Int
nVert = nSignals * nPoints

t :: HD.Vector Double
t = HD.linspace nPoints (-1, 1 :: Double)

coef :: Double
coef = 0.5 / fromIntegral nSignals :: Double

main :: IO ()
main = withDvzApp DVZ_BACKEND_GLFW $ \app -> do
  -- create the canvas, panel, and visual
  gpu <- dvz_gpu_best app
  canvas <- dvz_canvas gpu 2_560 1_440 0
  dvz_canvas_clear_color canvas 1 1 1
  scene <- dvz_scene canvas 1 1
  panel <- dvz_scene_panel scene 0 0 DVZ_CONTROLLER_PANZOOM 0
  visual <- dvz_scene_visual panel DVZ_VISUAL_LINE_STRIP 0
  let
    -- create the signals data

  y <- H.scale coef <$> H.rand nSignals nPoints
  userData <- VS.generateM nSignals $ \i -> do
    pure $ DVec3 (L.V3 (t `H.atIndex` i) (y `H.atIndex` (0, i)) 0.0)
  dvz_visual_data visual DVZ_PROP_POS 0 (fromIntegral nPoints) userData

  userDataColor <- VS.generateM nSignals $ \_ -> do
    dvz_colormap_scale DVZ_CMAP_VIRIDIS 0.5 0 1
  dvz_visual_data visual DVZ_PROP_COLOR 0 (fromIntegral nPoints) userDataColor

  dvz_event_callback canvas DVZ_EVENT_FRAME DVZ_EVENT_MODE_SYNC 0.0 userData $ \_ _ -> do
    y <- H.scale coef <$> H.rand nSignals nPoints
    userData <- VS.generateM nSignals $ \i -> do
      pure $ DVec3 (L.V3 (t `H.atIndex` i) (y `H.atIndex` (0, i)) 0.0)
    dvz_visual_data visual DVZ_PROP_POS 0 (fromIntegral nPoints) userData
    userDataColor <- VS.generateM nSignals $ \_ -> do
      dvz_colormap_scale DVZ_CMAP_VIRIDIS 0.5 0 1
    dvz_visual_data visual DVZ_PROP_COLOR 0 (fromIntegral nPoints) userDataColor
  dvz_app_run app 0


