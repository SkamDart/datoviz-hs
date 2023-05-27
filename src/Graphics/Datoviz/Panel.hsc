module Graphics.Datoviz.Panel where

import Foreign.Ptr

data C_DvzPanel

newtype DvzPanel = DvzPanel { getDvzPanel :: Ptr C_DvzPanel }
