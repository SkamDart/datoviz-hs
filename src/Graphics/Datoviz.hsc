{-# LANGUAGE PatternSynonyms #-}

module Graphics.Datoviz where

import Control.Exception
import Data.Word
import Foreign.C
import Foreign.Marshal.Array
import Foreign.Ptr

#include <datoviz/datoviz.h>

data C_DvzApp

newtype DvzApp = DvzApp { getDvzApp :: Ptr C_DvzApp }

newtype DvzBackend = DvzBackend { getDvzBackend :: CInt }

pattern DVZ_BACKEND_NONE :: DvzBackend
pattern DVZ_BACKEND_NONE = DvzBackend #{const DVZ_BACKEND_NONE}

pattern DVZ_BACKEND_GLFW :: DvzBackend
pattern DVZ_BACKEND_GLFW = DvzBackend #{const DVZ_BACKEND_GLFW}

pattern DVZ_BACKEND_OFFSCREEN :: DvzBackend
pattern DVZ_BACKEND_OFFSCREEN = DvzBackend #{const DVZ_BACKEND_OFFSCREEN}
{-# COMPLETE DVZ_BACKEND_NONE, DVZ_BACKEND_GLFW, DVZ_BACKEND_OFFSCREEN #-}

foreign import ccall dvz_app :: DvzBackend -> IO DvzApp

foreign import ccall dvz_app_run :: DvzApp -> Word64 -> IO ()

foreign import ccall dvz_app_destroy :: DvzApp -> IO ()

withDvzApp :: DvzBackend -> (DvzApp -> IO a) -> IO a
withDvzApp backend = bracket (dvz_app backend) dvz_app_destroy

data C_DvzGpu

newtype DvzGpu = DvzGpu { getDvzGpu :: Ptr C_DvzGpu }

foreign import ccall dvz_gpu_best :: DvzApp -> IO DvzGpu

data C_DvzCanvas

newtype DvzCanvas = DvzCanvas { getDvzCanvas :: Ptr C_DvzCanvas }

foreign import ccall dvz_canvas :: DvzGpu -> Word32 -> Word32 -> CInt -> IO DvzCanvas

data C_DvzScene

newtype DvzScene = DvzScene { getDvzScene :: Ptr C_DvzScene }

foreign import ccall dvz_scene :: DvzCanvas -> Word32 -> Word32 -> IO DvzScene

data C_DvzPanel

newtype DvzPanel = DvzPanel { getDvzPanel :: Ptr C_DvzPanel }

newtype DvzControllerType = DvzControllerType { getDvzControllerType :: CInt }

pattern DVZ_CONTROLLER_NONE :: DvzControllerType
pattern DVZ_CONTROLLER_NONE = DvzControllerType #{const DVZ_CONTROLLER_NONE}

pattern DVZ_CONTROLLER_PANZOOM :: DvzControllerType
pattern DVZ_CONTROLLER_PANZOOM = DvzControllerType #{const DVZ_CONTROLLER_PANZOOM}

pattern DVZ_CONTROLLER_AXES_2D :: DvzControllerType
pattern DVZ_CONTROLLER_AXES_2D = DvzControllerType #{const DVZ_CONTROLLER_AXES_2D}

pattern DVZ_CONTROLLER_ARCBALL :: DvzControllerType
pattern DVZ_CONTROLLER_ARCBALL = DvzControllerType #{const DVZ_CONTROLLER_ARCBALL}

pattern DVZ_CONTROLLER_CAMERA :: DvzControllerType
pattern DVZ_CONTROLLER_CAMERA = DvzControllerType #{const DVZ_CONTROLLER_CAMERA}

pattern DVZ_CONTROLLER_AXES_3D :: DvzControllerType
pattern DVZ_CONTROLLER_AXES_3D = DvzControllerType #{const DVZ_CONTROLLER_AXES_3D}
{-# COMPLETE DVZ_CONTROLLER_NONE, DVZ_CONTROLLER_PANZOOM, DVZ_CONTROLLER_AXES_2D
  , DVZ_CONTROLLER_ARCBALL, DVZ_CONTROLLER_CAMERA, DVZ_CONTROLLER_AXES_3D #-}

foreign import ccall dvz_scene_panel :: DvzScene -> Word32 -> Word32 -> DvzControllerType -> CInt -> IO DvzPanel

data C_DvzVisual

newtype DvzVisual = DvzVisual { getDvzVisual :: Ptr C_DvzVisual }

foreign import ccall dvz_blank_visual :: DvzScene -> CInt -> IO DvzVisual

data C_DvzGraphics

newtype DvzGraphics = DvzGraphics { getDvzGraphics :: Ptr C_DvzGraphics }

foreign import ccall dvz_blank_graphics :: DvzScene -> CInt -> IO DvzGraphics

foreign import ccall dvz_graphics_shader :: DvzGraphics -> CInt -> CString -> IO ()

foreign import ccall dvz_graphics_topology :: DvzGraphics -> CInt -> IO ()

foreign import ccall dvz_graphics_vertex_binding :: DvzGraphics -> Word32 -> Word64 -> IO ()

foreign import ccall dvz_graphics_vertex_attr :: DvzGraphics -> Word32 -> Word32 -> CInt -> Word64 -> IO ()

foreign import ccall dvz_custom_graphics :: DvzVisual -> DvzGraphics -> IO ()

foreign import ccall dvz_graphics_create :: DvzGraphics -> IO ()

foreign import ccall dvz_custom_visual :: DvzPanel -> DvzVisual -> IO ()

newtype DvzSourceType = DvzSourceType { getDvzSourceType :: CInt }

pattern DVZ_SOURCE_TYPE_NONE :: DvzSourceType
pattern DVZ_SOURCE_TYPE_NONE = DvzSourceType #{const DVZ_SOURCE_TYPE_NONE}

pattern DVZ_SOURCE_TYPE_MVP :: DvzSourceType
pattern DVZ_SOURCE_TYPE_MVP = DvzSourceType #{const DVZ_SOURCE_TYPE_MVP}

pattern DVZ_SOURCE_TYPE_VIEWPORT :: DvzSourceType
pattern DVZ_SOURCE_TYPE_VIEWPORT = DvzSourceType #{const DVZ_SOURCE_TYPE_VIEWPORT}

pattern DVZ_SOURCE_TYPE_PARAM :: DvzSourceType
pattern DVZ_SOURCE_TYPE_PARAM = DvzSourceType #{const DVZ_SOURCE_TYPE_PARAM}

pattern DVZ_SOURCE_TYPE_VERTEX :: DvzSourceType
pattern DVZ_SOURCE_TYPE_VERTEX = DvzSourceType #{const DVZ_SOURCE_TYPE_VERTEX}

pattern DVZ_SOURCE_TYPE_INDEX :: DvzSourceType
pattern DVZ_SOURCE_TYPE_INDEX = DvzSourceType #{const DVZ_SOURCE_TYPE_INDEX}

pattern DVZ_SOURCE_TYPE_IMAGE :: DvzSourceType
pattern DVZ_SOURCE_TYPE_IMAGE = DvzSourceType #{const DVZ_SOURCE_TYPE_IMAGE}

pattern DVZ_SOURCE_TYPE_VOLUME :: DvzSourceType
pattern DVZ_SOURCE_TYPE_VOLUME = DvzSourceType #{const DVZ_SOURCE_TYPE_VOLUME}

pattern DVZ_SOURCE_TYPE_TRANSFER :: DvzSourceType
pattern DVZ_SOURCE_TYPE_TRANSFER = DvzSourceType #{const DVZ_SOURCE_TYPE_TRANSFER}

pattern DVZ_SOURCE_TYPE_COLOR_TEXTURE :: DvzSourceType
pattern DVZ_SOURCE_TYPE_COLOR_TEXTURE = DvzSourceType #{const DVZ_SOURCE_TYPE_COLOR_TEXTURE}

pattern DVZ_SOURCE_TYPE_FONT_ATLAS :: DvzSourceType
pattern DVZ_SOURCE_TYPE_FONT_ATLAS = DvzSourceType #{const DVZ_SOURCE_TYPE_FONT_ATLAS}

pattern DVZ_SOURCE_TYPE_OTHER :: DvzSourceType
pattern DVZ_SOURCE_TYPE_OTHER = DvzSourceType #{const DVZ_SOURCE_TYPE_OTHER}

pattern DVZ_SOURCE_TYPE_COUNT :: DvzSourceType
pattern DVZ_SOURCE_TYPE_COUNT = DvzSourceType #{const DVZ_SOURCE_TYPE_COUNT}

{-# COMPLETE DVZ_SOURCE_TYPE_NONE,
              DVZ_SOURCE_TYPE_MVP,
              DVZ_SOURCE_TYPE_VIEWPORT,
              DVZ_SOURCE_TYPE_PARAM,
              DVZ_SOURCE_TYPE_VERTEX,
              DVZ_SOURCE_TYPE_INDEX,
              DVZ_SOURCE_TYPE_IMAGE,
              DVZ_SOURCE_TYPE_VOLUME,
              DVZ_SOURCE_TYPE_TRANSFER,
              DVZ_SOURCE_TYPE_COLOR_TEXTURE,
              DVZ_SOURCE_TYPE_FONT_ATLAS,
              DVZ_SOURCE_TYPE_OTHER,
              DVZ_SOURCE_TYPE_COUNT #-}

foreign import ccall dvz_visual_data_source :: DvzVisual -> DvzSourceType -> Word32 -> Word32 -> Word32 -> Word32 -> Ptr () -> IO ()

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

