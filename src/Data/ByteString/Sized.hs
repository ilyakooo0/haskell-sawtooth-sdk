module Data.ByteString.Sized
  ( ByteString,
    byteString,
    unByteString,
  )
where

import qualified Data.ByteString as BS
import Data.Proxy
import GHC.TypeLits

newtype ByteString (n :: Nat) = ByteString BS.ByteString

unByteString :: ByteString n -> BS.ByteString
unByteString (ByteString bs) = bs

byteString :: forall n. KnownNat n => BS.ByteString -> Maybe (ByteString n)
byteString bs | BS.length bs == fromInteger (natVal (Proxy @n)) = Just $ ByteString bs
byteString _ = Nothing
