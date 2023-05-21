{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE PatternSynonyms #-}

module Graphics.Datoviz.Scene where

import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Graphics.Datoviz.Canvas
import Graphics.Datoviz.Colormaps
import Graphics.Datoviz.Panel
import Graphics.Datoviz.Vklite
import Graphics.Datoviz.Visuals

#include <datoviz/datoviz.h>

data C_DvzScene

newtype DvzScene = DvzScene { getDvzScene :: Ptr C_DvzScene }

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

newtype DvzVisualFlags = DvzVisualFlags { getDvzVisualFlags :: CInt }

newtype DvzSceneUpdateType = DvzSceneUpdateType { getDvzSceneUpdateType :: CInt }

pattern DVZ_SCENE_UPDATE_NONE :: DvzSceneUpdateType
pattern DVZ_SCENE_UPDATE_NONE = DvzSceneUpdateType #{const DVZ_SCENE_UPDATE_NONE}

pattern DVZ_SCENE_UPDATE_VISUAL_ADDED :: DvzSceneUpdateType
pattern DVZ_SCENE_UPDATE_VISUAL_ADDED = DvzSceneUpdateType #{const DVZ_SCENE_UPDATE_VISUAL_ADDED }

pattern DVZ_SCENE_UPDATE_VISUAL_CHANGED :: DvzSceneUpdateType
pattern DVZ_SCENE_UPDATE_VISUAL_CHANGED = DvzSceneUpdateType #{const DVZ_SCENE_UPDATE_VISUAL_CHANGED }

pattern DVZ_SCENE_UPDATE_PROP_CHANGED :: DvzSceneUpdateType
pattern DVZ_SCENE_UPDATE_PROP_CHANGED = DvzSceneUpdateType #{const DVZ_SCENE_UPDATE_PROP_CHANGED }

pattern DVZ_SCENE_UPDATE_VISIBILITY_CHANGED :: DvzSceneUpdateType
pattern DVZ_SCENE_UPDATE_VISIBILITY_CHANGED = DvzSceneUpdateType #{const DVZ_SCENE_UPDATE_VISIBILITY_CHANGED }

pattern DVZ_SCENE_UPDATE_ITEM_COUNT_CHANGED :: DvzSceneUpdateType
pattern DVZ_SCENE_UPDATE_ITEM_COUNT_CHANGED = DvzSceneUpdateType #{const DVZ_SCENE_UPDATE_ITEM_COUNT_CHANGED }

pattern DVZ_SCENE_UPDATE_PANEL_CHANGED :: DvzSceneUpdateType
pattern DVZ_SCENE_UPDATE_PANEL_CHANGED = DvzSceneUpdateType #{const DVZ_SCENE_UPDATE_PANEL_CHANGED }

pattern DVZ_SCENE_UPDATE_INTERACT_CHANGED :: DvzSceneUpdateType
pattern DVZ_SCENE_UPDATE_INTERACT_CHANGED = DvzSceneUpdateType #{const DVZ_SCENE_UPDATE_INTERACT_CHANGED }

pattern DVZ_SCENE_UPDATE_COORDS_CHANGED :: DvzSceneUpdateType
pattern DVZ_SCENE_UPDATE_COORDS_CHANGED = DvzSceneUpdateType #{const DVZ_SCENE_UPDATE_COORDS_CHANGED }

{-# COMPLETE
      DVZ_SCENE_UPDATE_NONE
    , DVZ_SCENE_UPDATE_VISUAL_ADDED
    , DVZ_SCENE_UPDATE_VISUAL_CHANGED
    , DVZ_SCENE_UPDATE_PROP_CHANGED
    , DVZ_SCENE_UPDATE_VISIBILITY_CHANGED
    , DVZ_SCENE_UPDATE_ITEM_COUNT_CHANGED
    , DVZ_SCENE_UPDATE_PANEL_CHANGED
    , DVZ_SCENE_UPDATE_INTERACT_CHANGED
    , DVZ_SCENE_UPDATE_COORDS_CHANGED #-}

foreign import ccall dvz_scene :: DvzCanvas -> Word32 -> Word32 -> IO DvzScene
foreign import capi safe "datoviz/datoviz.h" dvz_scene_destroy :: DvzScene -> IO ()

foreign import ccall dvz_custom_graphics :: DvzVisual -> DvzGraphics -> IO ()
foreign import ccall dvz_custom_visual :: DvzPanel -> DvzVisual -> IO ()

foreign import ccall dvz_scene_panel :: DvzScene -> Word32 -> Word32 -> DvzControllerType -> CInt -> IO DvzPanel
foreign import ccall dvz_blank_visual :: DvzScene -> CInt -> IO DvzVisual
foreign import ccall dvz_blank_graphics :: DvzScene -> CInt -> IO DvzGraphics

foreign import capi safe "datoviz/datoviz.h" dvz_scene_visual :: DvzPanel -> DvzVisualType -> CInt -> IO DvzVisual

