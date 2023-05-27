{-# LANGUAGE CApiFFI #-}
module Graphics.Datoviz.Canvas where

import Data.Word
import Foreign.Ptr
import Foreign.C.Types
import Graphics.Datoviz.Vklite

data C_DvzCanvas

newtype DvzCanvas = DvzCanvas { getDvzCanvas :: Ptr C_DvzCanvas }

foreign import ccall dvz_canvas :: DvzGpu -> Word32 -> Word32 -> CInt -> IO DvzCanvas

foreign import capi safe "datoviz/datoviz.h" dvz_canvas_clear_color :: DvzCanvas -> CFloat -> CFloat -> CFloat -> IO ()
