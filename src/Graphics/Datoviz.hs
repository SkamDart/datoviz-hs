module Graphics.Datoviz
( module Graphics.Datoviz
, module Graphics.Datoviz.App
, module Graphics.Datoviz.Canvas
, module Graphics.Datoviz.Scene
, module Graphics.Datoviz.Types
, module Graphics.Datoviz.Visuals
, module Graphics.Datoviz.Vklite
) where

import Control.Exception

import Graphics.Datoviz.App
import Graphics.Datoviz.Canvas
import Graphics.Datoviz.Scene
import Graphics.Datoviz.Types
import Graphics.Datoviz.Visuals
import Graphics.Datoviz.Vklite

-- | Create a DvzApp with a provided backend.
withDvzApp :: DvzBackend -> (DvzApp -> IO a) -> IO a
withDvzApp backend = bracket (dvz_app backend) dvz_app_destroy
