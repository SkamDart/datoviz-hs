module Graphics.Datoviz.Vklite where

import Data.Word
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr

import Graphics.Datoviz.App

data C_DvzGpu

newtype DvzGpu = DvzGpu { getDvzGpu :: Ptr C_DvzGpu }

data C_DvzGraphics

newtype DvzGraphics = DvzGraphics { getDvzGraphics :: Ptr C_DvzGraphics }

foreign import ccall dvz_gpu_best :: DvzApp -> IO DvzGpu

foreign import ccall dvz_graphics_shader :: DvzGraphics -> CInt -> CString -> IO ()

foreign import ccall dvz_graphics_topology :: DvzGraphics -> CInt -> IO ()

foreign import ccall dvz_graphics_vertex_binding :: DvzGraphics -> Word32 -> Word64 -> IO ()

foreign import ccall dvz_graphics_vertex_attr :: DvzGraphics -> Word32 -> Word32 -> CInt -> Word64 -> IO ()

foreign import ccall dvz_graphics_create :: DvzGraphics -> IO ()

