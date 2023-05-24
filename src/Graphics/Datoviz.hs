{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Graphics.Datoviz
( module Graphics.Datoviz
, module Graphics.Datoviz.App
, module Graphics.Datoviz.Canvas
, module Graphics.Datoviz.Colormaps
, module Graphics.Datoviz.Scene
, module Graphics.Datoviz.Types
, module Graphics.Datoviz.Visuals
, module Graphics.Datoviz.Vklite
) where

import Control.Exception
import Data.Vector.Storable qualified as VS
import Data.Word

import Foreign (Storable, withForeignPtr, castPtr)

import Graphics.Datoviz.App
import Graphics.Datoviz.Canvas
import Graphics.Datoviz.Colormaps
import Graphics.Datoviz.Scene
import Graphics.Datoviz.Types
import Graphics.Datoviz.Visuals
import Graphics.Datoviz.Vklite
import Data.Coerce (coerce)

-- | Create a DvzApp with a provided backend.
withDvzApp :: DvzBackend -> (DvzApp -> IO a) -> IO a
withDvzApp backend = bracket (dvz_app backend) dvz_app_destroy

dvz_colormap_scale :: DvzColorMap -> Double -> Double -> Double -> CVec4 -> IO ()
dvz_colormap_scale d x y z v = do
    -- we need to help out the typechecker here.
    let cv = (coerce v :: VS.Vector Word8)
    withForeignPtr (fst $ VS.unsafeToForeignPtr0 cv) $ \v' -> do
        c_dvz_colormap_scale d x y z (castPtr v')

-- c_dvz_visual_data :: DvzVisual -> DvzPropType -> Word32 -> Word32 -> Ptr () -> IO ()
dvz_visual_data :: Storable a => DvzVisual -> DvzPropType -> Word32 -> Word32 -> VS.Vector a -> IO ()
dvz_visual_data dv p i c v =
    withForeignPtr (fst $ VS.unsafeToForeignPtr0 v) $ \d'-> do
        c_dvz_visual_data dv p i c (castPtr d')

