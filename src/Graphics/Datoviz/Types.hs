{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graphics.Datoviz.Types where

import Data.Int
import Data.Word
import Foreign.Storable
import Linear.V2 qualified as L
import Linear.V3 qualified as L
import Linear.V4 qualified as L

-- 8-bit integers
newtype CVec2 = CVec2 (L.V2 Word8)
    deriving (Eq, Show, Storable)
newtype CVec3 = CVec3 (L.V3 Word8)
    deriving (Eq, Show, Storable)
newtype CVec4 = CVec4 (L.V4 Word8)
    deriving (Eq, Show, Storable)

-- 16-bit integers
newtype SVec2 = SVec2 (L.V2 Int16)
    deriving (Eq, Show, Storable)
newtype SVec3 = SVec3 (L.V3 Int16)
    deriving (Eq, Show, Storable)
newtype SVec4 = SVec4 (L.V4 Int16)
    deriving (Eq, Show, Storable)

newtype UsVec2 = UsVec2 (L.V2 Word16)
    deriving (Eq, Show, Storable)
newtype UsVec3 = UsVec3 (L.V3 Word16)
    deriving (Eq, Show, Storable)
newtype UsVec4 = UsVec4 (L.V4 Word16)
    deriving (Eq, Show, Storable)

-- 32-bit integers
newtype IVec2 = IVec2 (L.V2 Int32)
    deriving (Eq, Show, Storable)
newtype IVec3 = IVec3 (L.V3 Int32)
    deriving (Eq, Show, Storable)
newtype IVec4 = IVec4 (L.V4 Int32)
    deriving (Eq, Show, Storable)

newtype UVec2 = UVec2 (L.V2 Word32)
    deriving (Eq, Show, Storable)
newtype UVec3 = UVec3 (L.V3 Word32)
    deriving (Eq, Show, Storable)
newtype UVec4 = UVec4 (L.V4 Word32)
    deriving (Eq, Show, Storable)

-- single precision floating point numbers
newtype FVec2 = FVec2 (L.V2 Float)
    deriving (Eq, Show, Storable)
newtype FVec3 = FVec3 (L.V3 Float)
    deriving (Eq, Show, Storable)
newtype FVec4 = FVec4 (L.V4 Float)
    deriving (Eq, Show, Storable)

-- double precision floating point numbers
newtype DVec2 = DVec2 (L.V2 Double)
    deriving (Eq, Show, Storable)
newtype DVec3 = DVec3 (L.V3 Double)
    deriving (Eq, Show, Storable)
newtype DVec4 = DVec4 (L.V4 Double)
    deriving (Eq, Show, Storable)
