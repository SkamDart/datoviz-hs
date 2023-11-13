{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE PatternSynonyms #-}

module Graphics.Datoviz.Canvas where

import qualified Data.Vector.Storable as VS
import Data.Word
import Foreign
import Foreign.C.Types
import Graphics.Datoviz.Vklite

#include <datoviz/canvas.h>

data C_DvzCanvas

newtype DvzCanvas = DvzCanvas { getDvzCanvas :: Ptr C_DvzCanvas }

foreign import ccall dvz_canvas :: DvzGpu -> Word32 -> Word32 -> CInt -> IO DvzCanvas

foreign import capi safe "datoviz/datoviz.h" dvz_canvas_clear_color :: DvzCanvas -> CFloat -> CFloat -> CFloat -> IO ()

-- Event callback.
--
-- The event callback is called when an event occurs on the canvas.
newtype DvzEventType = DvzEventType { getDvzEventType :: CInt }

-- | No event.
pattern DVZ_EVENT_NONE :: DvzEventType
pattern DVZ_EVENT_NONE = DvzEventType #{const DVZ_EVENT_NONE}

-- | Initialization event.
pattern DVZ_EVENT_INIT :: DvzEventType
pattern DVZ_EVENT_INIT = DvzEventType #{const DVZ_EVENT_INIT}

pattern DVZ_EVENT_REFILL :: DvzEventType
pattern DVZ_EVENT_REFILL = DvzEventType #{const DVZ_EVENT_REFILL}

pattern DVZ_EVENT_INTERACT :: DvzEventType
pattern DVZ_EVENT_INTERACT = DvzEventType #{const DVZ_EVENT_INTERACT}

-- | Frame event.
pattern DVZ_EVENT_FRAME :: DvzEventType
pattern DVZ_EVENT_FRAME = DvzEventType #{const DVZ_EVENT_FRAME }

pattern DVZ_EVENT_DESTROY :: DvzEventType
pattern DVZ_EVENT_DESTROY = DvzEventType #{const DVZ_EVENT_DESTROY}

-- Event mode.
--
-- The event mode determines whether the event callback is called synchronously or asynchronously.
newtype DvzEventMode = DvzEventMode { getDvzEventMode :: CInt }

-- | Synchronous event mode.
pattern DVZ_EVENT_MODE_SYNC :: DvzEventMode
pattern DVZ_EVENT_MODE_SYNC = DvzEventMode #{const DVZ_EVENT_MODE_SYNC}

-- Asynchronous event mode.
pattern DVZ_EVENT_MODE_ASYNC :: DvzEventMode
pattern DVZ_EVENT_MODE_ASYNC = DvzEventMode #{const DVZ_EVENT_MODE_ASYNC}

type DvzEventCallback = DvzCanvas -> DvzEventType -> IO ()

-- Wrapper function to create a Haskell closure that can be used as a C callback.
--
-- See the GHC documentation for more information:
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/ffi.html#using-foreign-export-and-foreign-import-ccall-wrapper-with-ghc
foreign import ccall safe "wrapper" wrap_dvz_event_callback 
  :: DvzEventCallback -> IO (FunPtr DvzEventCallback)

-- Register an event callback.
foreign import capi unsafe "datoviz/canvas.h dvz_event_callback" c_dvz_event_callback
  :: DvzCanvas
  -> DvzEventType
  -> CDouble
  -> DvzEventMode
  -> FunPtr DvzEventCallback
  -> Ptr ()
  -> IO ()

dvz_event_callback
  :: VS.Storable a
  => DvzCanvas
  -> DvzEventType
  -> DvzEventMode
  -> Double 
  -> VS.Vector a
  -> (DvzCanvas -> DvzEventType -> IO ())
  -> IO ()
dvz_event_callback canvas eventType eventMode x userData cb = do
  wrapper <- wrap_dvz_event_callback cb
  withForeignPtr (fst $ VS.unsafeToForeignPtr0 userData) $ \userData' ->
    c_dvz_event_callback canvas eventType (realToFrac x) eventMode wrapper (castPtr userData')
