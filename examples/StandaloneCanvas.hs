{-# LANGUAGE CApiFFI #-}
module Main where

import Data.Word

import Graphics.Datoviz
import Foreign (Ptr)

foreign import capi unsafe "datoviz/datoviz.h" dvz_rand_normal :: IO Float
foreign import capi unsafe "datoviz/datoviz.h" dvz_rand_float :: IO Float

main :: IO ()
main = do
    -- We create a singleton application with the GLFW backend.
    withDvzApp DVZ_BACKEND_GLFW $ \app -> do
        -- Get the best GPU for the job.
        gpu <- dvz_gpu_best app

        -- Create a canvas with a specified size.
        canvas <- dvz_canvas gpu 2560 1440 0

        -- We use a white background color (RGB floating-point values in [0, 1]).
        dvz_canvas_clear_color canvas 1 1 1

        -- We create a scene, which allows us to define several subplots (panels) organized within a
        -- grid. Here, we just use a single panel spanning the entire canvas.
        scene <- dvz_scene canvas 1 1

        -- We get the panel at row 0, column 0, and we initialize it with an axes 2D controller.
        -- The last argument is for optional flags.
        panel <- dvz_scene_panel scene 0 0 DVZ_CONTROLLER_AXES_2D 0

        -- We add a new "marker" visual in the panel.
        -- The last argument is for optional flag
        visual <- dvz_scene_visual panel  DVZ_VISUAL_MARKER 0

        -- visual data but we don't do for-loops here
        let n = 10000
        pos <- mkPosVec n
        size <- mkSizeVec n
        color <- mkColorVec n
        mapM_ (\(p, d) -> dvz_visual_data visual p 0 n d) [(DVZ_PROP_POS, pos), (DVZ_PROP_COLOR, color), (DVZ_PROP_MARKER_SIZE, size)]
        dvz_app_run app 0

mkColorVec :: Word32 -> IO (Ptr DVe)
mkColorVec n = do


mkSizeVec :: Word32 -> IO (Ptr ())
mkSizeVec n = undefined

mkPosVec :: Word32 -> IO (Ptr ())
mkPosVec n = undefined
