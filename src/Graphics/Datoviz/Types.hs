{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Datoviz.Types where

import Data.Data
import Data.Vector.Storable qualified as VS
import Foreign
import GHC.TypeNats

-- | Fixed-Sized Vector with type `a` and length `b`.
newtype FsVec a (b :: Nat) = FsVec (VS.Vector a)

-- 8-bit integers
type CVec2 = FsVec Word8 2
type CVec3 = FsVec Word8 3
type CVec4 = FsVec Word8 4

-- 16-bit integers
type SVec2 = FsVec Int16 2
type SVec3 = FsVec Int16 3
type SVec4 = FsVec Int16 4

type UsVec2 = FsVec Word16 2
type UsVec3 = FsVec Word16 3
type UsVec4 = FsVec Word16 4

-- 32-bit integers
type IVec2 = FsVec Int32 2
type IVec3 = FsVec Int32 3
type IVec4 = FsVec Int32 4

type UVec2 = FsVec Word32 2
type UVec3 = FsVec Word32 3
type UVec4 = FsVec Word32 4

-- single precision floating point numbers
type FVec2 = FsVec Float 2
type FVec3 = FsVec Float 3
type FVec4 = FsVec Float 4

-- double precision floating point numbers
type DVec2 = FsVec Double 2
type DVec3 = FsVec Double 3
type DVec4 = FsVec Double 4

instance (Storable a, KnownNat b) => Storable (FsVec a b) where
    sizeOf _ = fromIntegral (natVal (Proxy @b)) * sizeOf (undefined :: a)
    alignment _ = alignment (undefined :: a)
    peek p = undefined
    poke p cv = undefined
