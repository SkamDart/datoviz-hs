{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Graphics.Datoviz.Types where

import Foreign (Storable)
import GHC.TypeNats

newtype CVec a (b :: Nat) = CVec (V.Vector a)

instance (Storable a, KnownNat b) => Storable (CVec a b) where
