{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Graphics.Datoviz.Types where

import Data.Coerce (coerce)
import Data.Data
import Data.Vector.Storable qualified as VS
import Foreign
import GHC.TypeNats

-- | Fixed-Sized Vector with type `a` and length `b`.
newtype FsVec a (b :: Nat) = FsVec (VS.Vector a)
    deriving (Eq, Show)

-- 8-bit integers
newtype CVec2 = CVec2 (FsVec Word8 2)
    deriving (Eq, Show, Storable)
newtype CVec3 = CVec3 (FsVec Word8 3)
    deriving (Eq, Show, Storable)
newtype CVec4 = CVec4 (FsVec Word8 4)
    deriving (Eq, Show, Storable)

-- 16-bit integers
newtype SVec2 = SVec2 (FsVec Int16 2)
    deriving (Eq, Show, Storable)
newtype SVec3 = SVec3 (FsVec Int16 3)
    deriving (Eq, Show, Storable)
newtype SVec4 = SVec4 (FsVec Int16 4)
    deriving (Eq, Show, Storable)

newtype UsVec2 = UsVec2 (FsVec Word16 2)
    deriving (Eq, Show, Storable)
newtype UsVec3 = UsVec3 (FsVec Word16 3)
    deriving (Eq, Show, Storable)
newtype UsVec4 = UsVec4 (FsVec Word16 4)
    deriving (Eq, Show, Storable)

-- 32-bit integers
newtype IVec2 = IVec2 (FsVec Int32 2)
    deriving (Eq, Show, Storable)
newtype IVec3 = IVec3 (FsVec Int32 3)
    deriving (Eq, Show, Storable)
newtype IVec4 = IVec4 (FsVec Int32 4)
    deriving (Eq, Show, Storable)

newtype UVec2 = UVec2 (FsVec Word32 2)
    deriving (Eq, Show, Storable)
newtype UVec3 = UVec3 (FsVec Word32 3)
    deriving (Eq, Show, Storable)
newtype UVec4 = UVec4 (FsVec Word32 4)
    deriving (Eq, Show, Storable)

-- single precision floating point numbers
newtype FVec2 = FVec2 (FsVec Float 2)
    deriving (Eq, Show, Storable)
newtype FVec3 = FVec3 (FsVec Float 3)
    deriving (Eq, Show, Storable)
newtype FVec4 = FVec4 (FsVec Float 4)
    deriving (Eq, Show, Storable)

-- double precision floating point numbers
newtype DVec2 = DVec2 (FsVec Double 2)
    deriving (Eq, Show, Storable)
newtype DVec3 = DVec3 (FsVec Double 3)
    deriving (Eq, Show, Storable)
newtype DVec4 = DVec4 (FsVec Double 4)
    deriving (Eq, Show, Storable)

instance (Storable a, KnownNat b) => Storable (FsVec a b) where
    sizeOf _ = fromIntegral (natVal (Proxy @b)) * sizeOf (undefined :: a)
    alignment _ = alignment (undefined :: a)
    peek srcPtr = do
        -- this is a bit sketchy but walk through this with me.
        --
        -- len is the value-level of b.
        let len = fromIntegral (natVal (Proxy @b))
        -- malloc a foreign ptr with enough bytes to fit a (FsVec a b)
        destFp <- mallocForeignPtrBytes (sizeOf (undefined :: (FsVec a b)))
        withForeignPtr destFp $ \ destPtr -> do
            -- copy from our srcPtr to the destPtr `len` elements with sizeOf (undefined :: a)
            copyArray srcPtr destPtr len
        -- create a storable vector from the malloc'ed ForeignPtr above.
        pure $ FsVec $ VS.unsafeFromForeignPtr0 (castForeignPtr destFp) len
    poke ptr fsv = do
        withForeignPtr (fst $ VS.unsafeToForeignPtr0 (coerce fsv)) $ \vptr -> do
            moveArray ptr vptr (fromIntegral (natVal (Proxy @b)))
