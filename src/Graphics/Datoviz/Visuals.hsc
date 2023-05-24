{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE PatternSynonyms #-}

module Graphics.Datoviz.Visuals where

import Data.Word
import Foreign.C.Types
import Foreign.Ptr

#include <datoviz/datoviz.h>

data C_DvzVisual

newtype DvzVisual = DvzVisual { getDvzVisual :: Ptr C_DvzVisual }

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

newtype DvzVisualType = DvzVisualType { getDvzVisualType :: CInt }

pattern DVZ_VISUAL_NONE :: DvzVisualType
pattern DVZ_VISUAL_NONE = DvzVisualType #{const DVZ_VISUAL_NONE}
pattern DVZ_VISUAL_POINT :: DvzVisualType
pattern DVZ_VISUAL_POINT = DvzVisualType #{const DVZ_VISUAL_POINT}
pattern DVZ_VISUAL_LINE :: DvzVisualType
pattern DVZ_VISUAL_LINE = DvzVisualType #{const DVZ_VISUAL_LINE}
pattern DVZ_VISUAL_LINE_STRIP :: DvzVisualType
pattern DVZ_VISUAL_LINE_STRIP = DvzVisualType #{const DVZ_VISUAL_LINE_STRIP}
pattern DVZ_VISUAL_TRIANGLE :: DvzVisualType
pattern DVZ_VISUAL_TRIANGLE = DvzVisualType #{const DVZ_VISUAL_TRIANGLE}
pattern DVZ_VISUAL_TRIANGLE_STRIP :: DvzVisualType
pattern DVZ_VISUAL_TRIANGLE_STRIP = DvzVisualType #{const DVZ_VISUAL_TRIANGLE_STRIP}
pattern DVZ_VISUAL_TRIANGLE_FAN :: DvzVisualType
pattern DVZ_VISUAL_TRIANGLE_FAN = DvzVisualType #{const DVZ_VISUAL_TRIANGLE_FAN}
pattern DVZ_VISUAL_RECTANGLE :: DvzVisualType
pattern DVZ_VISUAL_RECTANGLE = DvzVisualType #{const DVZ_VISUAL_RECTANGLE}
pattern DVZ_VISUAL_MARKER :: DvzVisualType
pattern DVZ_VISUAL_MARKER = DvzVisualType #{const DVZ_VISUAL_MARKER}
pattern DVZ_VISUAL_SEGMENT :: DvzVisualType
pattern DVZ_VISUAL_SEGMENT = DvzVisualType #{const DVZ_VISUAL_SEGMENT}
pattern DVZ_VISUAL_ARROW :: DvzVisualType
pattern DVZ_VISUAL_ARROW = DvzVisualType #{const DVZ_VISUAL_ARROW}
pattern DVZ_VISUAL_PATH :: DvzVisualType
pattern DVZ_VISUAL_PATH = DvzVisualType #{const DVZ_VISUAL_PATH}
pattern DVZ_VISUAL_TEXT :: DvzVisualType
pattern DVZ_VISUAL_TEXT = DvzVisualType #{const DVZ_VISUAL_TEXT}
pattern DVZ_VISUAL_IMAGE :: DvzVisualType
pattern DVZ_VISUAL_IMAGE = DvzVisualType #{const DVZ_VISUAL_IMAGE}
pattern DVZ_VISUAL_IMAGE_CMAP :: DvzVisualType
pattern DVZ_VISUAL_IMAGE_CMAP = DvzVisualType #{const DVZ_VISUAL_IMAGE_CMAP}
pattern DVZ_VISUAL_DISC :: DvzVisualType
pattern DVZ_VISUAL_DISC = DvzVisualType #{const DVZ_VISUAL_DISC}
pattern DVZ_VISUAL_SECTOR :: DvzVisualType
pattern DVZ_VISUAL_SECTOR = DvzVisualType #{const DVZ_VISUAL_SECTOR}
pattern DVZ_VISUAL_MESH :: DvzVisualType
pattern DVZ_VISUAL_MESH = DvzVisualType #{const DVZ_VISUAL_MESH}
pattern DVZ_VISUAL_POLYGON :: DvzVisualType
pattern DVZ_VISUAL_POLYGON = DvzVisualType #{const DVZ_VISUAL_POLYGON}
pattern DVZ_VISUAL_PSLG :: DvzVisualType
pattern DVZ_VISUAL_PSLG = DvzVisualType #{const DVZ_VISUAL_PSLG}
pattern DVZ_VISUAL_HISTOGRAM :: DvzVisualType
pattern DVZ_VISUAL_HISTOGRAM = DvzVisualType #{const DVZ_VISUAL_HISTOGRAM}
pattern DVZ_VISUAL_AREA :: DvzVisualType
pattern DVZ_VISUAL_AREA = DvzVisualType #{const DVZ_VISUAL_AREA}
pattern DVZ_VISUAL_CANDLE :: DvzVisualType
pattern DVZ_VISUAL_CANDLE = DvzVisualType #{const DVZ_VISUAL_CANDLE}
pattern DVZ_VISUAL_GRAPH :: DvzVisualType
pattern DVZ_VISUAL_GRAPH = DvzVisualType #{const DVZ_VISUAL_GRAPH}
pattern DVZ_VISUAL_SURFACE :: DvzVisualType
pattern DVZ_VISUAL_SURFACE = DvzVisualType #{const DVZ_VISUAL_SURFACE}
pattern DVZ_VISUAL_VOLUME_SLICE :: DvzVisualType
pattern DVZ_VISUAL_VOLUME_SLICE = DvzVisualType #{const DVZ_VISUAL_VOLUME_SLICE}
pattern DVZ_VISUAL_VOLUME :: DvzVisualType
pattern DVZ_VISUAL_VOLUME = DvzVisualType #{const DVZ_VISUAL_VOLUME}
pattern DVZ_VISUAL_FAKE_SPHERE :: DvzVisualType
pattern DVZ_VISUAL_FAKE_SPHERE = DvzVisualType #{const DVZ_VISUAL_FAKE_SPHERE}
pattern DVZ_VISUAL_AXES_2D :: DvzVisualType
pattern DVZ_VISUAL_AXES_2D = DvzVisualType #{const DVZ_VISUAL_AXES_2D}
pattern DVZ_VISUAL_AXES_3D :: DvzVisualType
pattern DVZ_VISUAL_AXES_3D = DvzVisualType #{const DVZ_VISUAL_AXES_3D}
pattern DVZ_VISUAL_COLORMAP :: DvzVisualType
pattern DVZ_VISUAL_COLORMAP = DvzVisualType #{const DVZ_VISUAL_COLORMAP}
pattern DVZ_VISUAL_COUNT :: DvzVisualType
pattern DVZ_VISUAL_COUNT = DvzVisualType #{const DVZ_VISUAL_COUNT}
pattern DVZ_VISUAL_CUSTOM :: DvzVisualType
pattern DVZ_VISUAL_CUSTOM = DvzVisualType #{const DVZ_VISUAL_CUSTOM}

{-# COMPLETE DVZ_VISUAL_NONE,
    DVZ_VISUAL_POINT,
    DVZ_VISUAL_LINE,
    DVZ_VISUAL_LINE_STRIP,
    DVZ_VISUAL_TRIANGLE,
    DVZ_VISUAL_TRIANGLE_STRIP,
    DVZ_VISUAL_TRIANGLE_FAN,
    DVZ_VISUAL_RECTANGLE,
    DVZ_VISUAL_MARKER,
    DVZ_VISUAL_SEGMENT,
    DVZ_VISUAL_ARROW,
    DVZ_VISUAL_PATH,
    DVZ_VISUAL_TEXT,
    DVZ_VISUAL_IMAGE,
    DVZ_VISUAL_IMAGE_CMAP,
    DVZ_VISUAL_DISC,
    DVZ_VISUAL_SECTOR,
    DVZ_VISUAL_MESH,
    DVZ_VISUAL_POLYGON,
    DVZ_VISUAL_PSLG,
    DVZ_VISUAL_HISTOGRAM,
    DVZ_VISUAL_AREA,
    DVZ_VISUAL_CANDLE,
    DVZ_VISUAL_GRAPH,
    DVZ_VISUAL_SURFACE,
    DVZ_VISUAL_VOLUME_SLICE,
    DVZ_VISUAL_VOLUME,
    DVZ_VISUAL_FAKE_SPHERE,
    DVZ_VISUAL_AXES_2D,
    DVZ_VISUAL_AXES_3D,
    DVZ_VISUAL_COLORMAP,
    DVZ_VISUAL_COUNT,
    DVZ_VISUAL_CUSTOM
    #-}

newtype DvzPropType = DvzPropType { getDvzPropType :: CInt }

pattern DVZ_PROP_NONE :: DvzPropType
pattern DVZ_PROP_NONE = DvzPropType #{const DVZ_PROP_NONE}
pattern DVZ_PROP_POS :: DvzPropType
pattern DVZ_PROP_POS = DvzPropType #{const DVZ_PROP_POS}
pattern DVZ_PROP_COLOR :: DvzPropType
pattern DVZ_PROP_COLOR = DvzPropType #{const DVZ_PROP_COLOR}
pattern DVZ_PROP_ALPHA :: DvzPropType
pattern DVZ_PROP_ALPHA = DvzPropType #{const DVZ_PROP_ALPHA}
pattern DVZ_PROP_COLORMAP :: DvzPropType
pattern DVZ_PROP_COLORMAP = DvzPropType #{const DVZ_PROP_COLORMAP}
pattern DVZ_PROP_MARKER_SIZE :: DvzPropType
pattern DVZ_PROP_MARKER_SIZE = DvzPropType #{const DVZ_PROP_MARKER_SIZE}
pattern DVZ_PROP_MARKER_TYPE :: DvzPropType
pattern DVZ_PROP_MARKER_TYPE = DvzPropType #{const DVZ_PROP_MARKER_TYPE}
pattern DVZ_PROP_ANGLE :: DvzPropType
pattern DVZ_PROP_ANGLE = DvzPropType #{const DVZ_PROP_ANGLE}
pattern DVZ_PROP_TEXT :: DvzPropType
pattern DVZ_PROP_TEXT = DvzPropType #{const DVZ_PROP_TEXT}
pattern DVZ_PROP_TEXT_SIZE :: DvzPropType
pattern DVZ_PROP_TEXT_SIZE = DvzPropType #{const DVZ_PROP_TEXT_SIZE}
pattern DVZ_PROP_GLYPH :: DvzPropType
pattern DVZ_PROP_GLYPH = DvzPropType #{const DVZ_PROP_GLYPH}
pattern DVZ_PROP_ANCHOR :: DvzPropType
pattern DVZ_PROP_ANCHOR = DvzPropType #{const DVZ_PROP_ANCHOR}
pattern DVZ_PROP_LINE_WIDTH :: DvzPropType
pattern DVZ_PROP_LINE_WIDTH = DvzPropType #{const DVZ_PROP_LINE_WIDTH}
pattern DVZ_PROP_MITER_LIMIT :: DvzPropType
pattern DVZ_PROP_MITER_LIMIT = DvzPropType #{const DVZ_PROP_MITER_LIMIT}
pattern DVZ_PROP_CAP_TYPE :: DvzPropType
pattern DVZ_PROP_CAP_TYPE = DvzPropType #{const DVZ_PROP_CAP_TYPE}
pattern DVZ_PROP_JOIN_TYPE :: DvzPropType
pattern DVZ_PROP_JOIN_TYPE = DvzPropType #{const DVZ_PROP_JOIN_TYPE}
pattern DVZ_PROP_TOPOLOGY :: DvzPropType
pattern DVZ_PROP_TOPOLOGY = DvzPropType #{const DVZ_PROP_TOPOLOGY}
pattern DVZ_PROP_LENGTH :: DvzPropType
pattern DVZ_PROP_LENGTH = DvzPropType #{const DVZ_PROP_LENGTH}
pattern DVZ_PROP_RANGE :: DvzPropType
pattern DVZ_PROP_RANGE = DvzPropType #{const DVZ_PROP_RANGE}
pattern DVZ_PROP_MARGIN :: DvzPropType
pattern DVZ_PROP_MARGIN = DvzPropType #{const DVZ_PROP_MARGIN}
pattern DVZ_PROP_NORMAL :: DvzPropType
pattern DVZ_PROP_NORMAL = DvzPropType #{const DVZ_PROP_NORMAL}
pattern DVZ_PROP_TEXCOORDS :: DvzPropType
pattern DVZ_PROP_TEXCOORDS = DvzPropType #{const DVZ_PROP_TEXCOORDS}
pattern DVZ_PROP_TEXCOEFS :: DvzPropType
pattern DVZ_PROP_TEXCOEFS = DvzPropType #{const DVZ_PROP_TEXCOEFS}
pattern DVZ_PROP_TRANSFER_X :: DvzPropType
pattern DVZ_PROP_TRANSFER_X = DvzPropType #{const DVZ_PROP_TRANSFER_X}
pattern DVZ_PROP_TRANSFER_Y :: DvzPropType
pattern DVZ_PROP_TRANSFER_Y = DvzPropType #{const DVZ_PROP_TRANSFER_Y}
pattern DVZ_PROP_LIGHT_POS :: DvzPropType
pattern DVZ_PROP_LIGHT_POS = DvzPropType #{const DVZ_PROP_LIGHT_POS}
pattern DVZ_PROP_LIGHT_PARAMS :: DvzPropType
pattern DVZ_PROP_LIGHT_PARAMS = DvzPropType #{const DVZ_PROP_LIGHT_PARAMS}
pattern DVZ_PROP_CLIP :: DvzPropType
pattern DVZ_PROP_CLIP = DvzPropType #{const DVZ_PROP_CLIP}
pattern DVZ_PROP_MODEL :: DvzPropType
pattern DVZ_PROP_MODEL = DvzPropType #{const DVZ_PROP_MODEL}
pattern DVZ_PROP_VIEW :: DvzPropType
pattern DVZ_PROP_VIEW = DvzPropType #{const DVZ_PROP_VIEW}
pattern DVZ_PROP_PROJ :: DvzPropType
pattern DVZ_PROP_PROJ = DvzPropType #{const DVZ_PROP_PROJ}
pattern DVZ_PROP_VIEWPORT :: DvzPropType
pattern DVZ_PROP_VIEWPORT = DvzPropType #{const DVZ_PROP_VIEWPORT}
pattern DVZ_PROP_TIME :: DvzPropType
pattern DVZ_PROP_TIME = DvzPropType #{const DVZ_PROP_TIME}
pattern DVZ_PROP_INDEX :: DvzPropType
pattern DVZ_PROP_INDEX = DvzPropType #{const DVZ_PROP_INDEX}
pattern DVZ_PROP_SCALE :: DvzPropType
pattern DVZ_PROP_SCALE = DvzPropType #{const DVZ_PROP_SCALE}
pattern DVZ_PROP_TRANSFORM :: DvzPropType
pattern DVZ_PROP_TRANSFORM = DvzPropType #{const DVZ_PROP_TRANSFORM}

{-# COMPLETE
    DVZ_PROP_NONE
  , DVZ_PROP_POS
  , DVZ_PROP_COLOR
  , DVZ_PROP_ALPHA
  , DVZ_PROP_COLORMAP
  , DVZ_PROP_MARKER_SIZE
  , DVZ_PROP_MARKER_TYPE
  , DVZ_PROP_ANGLE
  , DVZ_PROP_TEXT
  , DVZ_PROP_TEXT_SIZE
  , DVZ_PROP_GLYPH
  , DVZ_PROP_ANCHOR
  , DVZ_PROP_LINE_WIDTH
  , DVZ_PROP_MITER_LIMIT
  , DVZ_PROP_CAP_TYPE
  , DVZ_PROP_JOIN_TYPE
  , DVZ_PROP_TOPOLOGY
  , DVZ_PROP_LENGTH
  , DVZ_PROP_RANGE
  , DVZ_PROP_MARGIN
  , DVZ_PROP_NORMAL
  , DVZ_PROP_TEXCOORDS
  , DVZ_PROP_TEXCOEFS
  , DVZ_PROP_TRANSFER_X
  , DVZ_PROP_TRANSFER_Y
  , DVZ_PROP_LIGHT_POS
  , DVZ_PROP_LIGHT_PARAMS
  , DVZ_PROP_CLIP
  , DVZ_PROP_MODEL
  , DVZ_PROP_VIEW
  , DVZ_PROP_PROJ
  , DVZ_PROP_VIEWPORT
  , DVZ_PROP_TIME
  , DVZ_PROP_INDEX
  , DVZ_PROP_SCALE
  , DVZ_PROP_TRANSFORM #-}

foreign import ccall dvz_visual_data_source :: DvzVisual -> DvzSourceType -> Word32 -> Word32 -> Word32 -> Word32 -> Ptr () -> IO ()

foreign import capi safe "datoviz/datoviz.h dvz_visual_data"
    c_dvz_visual_data :: DvzVisual -> DvzPropType -> Word32 -> Word32 -> Ptr () -> IO ()
