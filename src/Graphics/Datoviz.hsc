{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}

module Graphics.Datoviz
( module Graphics.Datoviz
, module Graphics.Datoviz.App
, module Graphics.Datoviz.Canvas
, module Graphics.Datoviz.Scene
, module Graphics.Datoviz.Visuals
, module Graphics.Datoviz.Vklite
) where

import Control.Exception
import Foreign.C
import Foreign.Marshal.Array
import Foreign.Ptr

import Graphics.Datoviz.App
import Graphics.Datoviz.Canvas
import Graphics.Datoviz.Scene
import Graphics.Datoviz.Visuals
import Graphics.Datoviz.Vklite

-- eventually we should move this out so this export becomes 'pure' haskell
#include <vulkan/vulkan_core.h>

-- | Create a DvzApp with a provided backend.
withDvzApp :: DvzBackend -> (DvzApp -> IO a) -> IO a
withDvzApp backend = bracket (dvz_app backend) dvz_app_destroy

mandelbrot :: IO ()
mandelbrot = withDvzApp DVZ_BACKEND_GLFW $ \app -> do
  gpu <- dvz_gpu_best app
  canvas <- dvz_canvas gpu 2560 1440 0
  scene <- dvz_scene canvas 1 1
  panel <- dvz_scene_panel scene 0 0 DVZ_CONTROLLER_PANZOOM 0
  visual <- dvz_blank_visual scene 0
  graphics <- dvz_blank_graphics scene 0
  withCString "./mandelbrot.vert.spv" $ \vertstr ->
    withCString "./mandelbrot.frag.spv" $ \fragstr -> do
      dvz_graphics_shader graphics #{const VK_SHADER_STAGE_VERTEX_BIT} vertstr
      dvz_graphics_shader graphics #{const VK_SHADER_STAGE_FRAGMENT_BIT} fragstr
      dvz_graphics_topology graphics #{const VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP}
      dvz_graphics_vertex_binding graphics 0 12
      dvz_graphics_vertex_attr graphics 0 0 #{const VK_FORMAT_R32G32B32_SFLOAT} 0
      dvz_graphics_create graphics
      dvz_custom_graphics visual graphics
      dvz_custom_visual panel visual
      let vertices = [-1, -1, 0, 1, -1, 0, -1, 1, 0, 1, 1, 0 :: Float]
      withArray vertices $ \a -> do
        dvz_visual_data_source visual DVZ_SOURCE_TYPE_VERTEX 0 0 4 4 (castPtr a)
        dvz_app_run app 0

