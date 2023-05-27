{-# LANGUAGE PatternSynonyms #-}
module Graphics.Datoviz.App where

import Data.Word
import Foreign.Ptr
import Foreign.C.Types

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
