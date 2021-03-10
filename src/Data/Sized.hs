module Data.Sized
  ( SizedType (..),
    TypeSize,
  )
where

import Data.ByteString.Sized
import GHC.TypeLits

-- | The encoded size of the type in bytes
type family TypeSize s :: Nat

class SizedType s where
  encodeSized :: s -> ByteString (TypeSize s)
  decodeSized :: ByteString (TypeSize s) -> Maybe s

type instance TypeSize (ByteString n) = n

instance SizedType (ByteString n) where
  encodeSized = id
  decodeSized = Just
