{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Numeric.Natural.Sized
  ( SizedNat,
    sizedNat,
    unSizedNat,
    encodeInt64,
    zeroNat,
    Natural,
    liftSizedNat,
    fmapSizedNat,
  )
where

import Control.Sawtooth.Transaction
import Data.Bits
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Sized as BSS
import Data.Maybe
import Data.Proxy
import Data.Sized
import Data.Word
import GHC.TypeLits
import Numeric.Natural

newtype SizedNat (n :: Nat) = SizedNat' Natural
  deriving newtype (Eq, Ord, Show)

instance KnownNat n => Serialise (SizedNat n) where
  encode (SizedNat' n) = encode n
  decode = decode >>= maybe (fail "Couldn't decode SizedNat") pure . sizedNat

zeroNat :: SizedNat n
zeroNat = SizedNat' 0

liftSizedNat :: KnownNat n => (Natural -> Natural -> Natural) -> SizedNat n -> SizedNat n -> Maybe (SizedNat n)
liftSizedNat f (SizedNat' x) (SizedNat' y) = sizedNat $ x `f` y

fmapSizedNat :: KnownNat n => (Natural -> Natural) -> SizedNat n -> Maybe (SizedNat n)
fmapSizedNat f (SizedNat' x) = sizedNat $ f x

instance KnownNat n => Bounded (SizedNat n) where
  minBound = SizedNat' 0
  maxBound = SizedNat' $ (2 ^ (8 * natVal (Proxy @n))) - 1

sizedNat :: forall n. KnownNat n => Natural -> Maybe (SizedNat n)
sizedNat n | n < (2 ^ (8 * natVal (Proxy @n))) = Just $ SizedNat' n
sizedNat _ = Nothing

unSizedNat :: SizedNat n -> Natural
unSizedNat (SizedNat' n) = n

encodeInt64 :: forall n. (KnownNat n, n <= 8) => SizedNat n -> BSS.ByteString n
encodeInt64 (SizedNat' nat) = fromJust . BSS.byteString . BS.drop (8 - fromIntegral (natVal (Proxy @n))) . word64B $ fromIntegral nat

word64B :: Word64 -> BS.ByteString
word64B = BSL.toStrict . BS.toLazyByteString . word64BE

word64 :: [Word8] -> Word64
word64 (x : xs) = fromIntegral x .|. shiftL (word64 xs) 8
word64 [] = 0

type instance TypeSize (SizedNat n) = n

-- This is a half-measure because I am lazy. Ideally we should have a generic implementation not limited to 64 bits.
instance (KnownNat n, n <= 8) => SizedType (SizedNat n) where
  encodeSized = encodeInt64
  decodeSized = sizedNat . fromIntegral . word64 . reverse . BS.unpack . BSS.unByteString
