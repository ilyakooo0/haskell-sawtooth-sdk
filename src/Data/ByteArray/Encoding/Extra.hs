module Data.ByteArray.Encoding.Extra
  ( toHexT,
    toHexBS,
    toHexTL,
    fromHexT,
    fromHexTL,
    showKeyHexT,
    showKeyHexTL,
    showPubKeyHexT,
    showPubKeyHexTL,
    to64T,
    to64URLT,
    from64URL,
    from64T,
    from64URLT,
    to64S,
    to64URLS,
    from64S,
    from64URLS,
    from64,
    to64,
  )
where

import Crypto.Secp256k1
import Data.Bifunctor
import Data.ByteArray
import Data.ByteArray.Encoding
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

toHexBS :: ByteArrayAccess bin => bin -> ByteString
toHexBS = convertToBase Base16

toHexT :: ByteArrayAccess bin => bin -> T.Text
toHexT = T.decodeUtf8 . convertToBase Base16

to64T :: ByteArrayAccess bin => bin -> T.Text
to64T = T.decodeUtf8 . convertToBase Base64

to64 :: (ByteArrayAccess bin, ByteArray x) => bin -> x
to64 = convertToBase Base64

to64URLT :: ByteArrayAccess bin => bin -> T.Text
to64URLT = T.decodeUtf8 . convertToBase Base64URLUnpadded

to64S :: ByteArrayAccess bin => bin -> String
to64S = BSC.unpack . convertToBase Base64

to64URLS :: ByteArrayAccess bin => bin -> String
to64URLS = BSC.unpack . convertToBase Base64URLUnpadded

toHexTL :: ByteArrayAccess bin => bin -> TL.Text
toHexTL = TL.decodeUtf8 . BSL.fromStrict . convertToBase Base16

fromHexT :: ByteArray x => T.Text -> Either T.Text x
fromHexT = first T.pack . convertFromBase Base16 . T.encodeUtf8

from64T :: ByteArray x => T.Text -> Either T.Text x
from64T = first T.pack . convertFromBase Base64 . T.encodeUtf8

from64 :: (ByteArray x, ByteArrayAccess bin) => bin -> Either T.Text x
from64 = first T.pack . convertFromBase Base64

from64URL :: (ByteArray x, ByteArrayAccess bin) => bin -> Either T.Text x
from64URL = first T.pack . convertFromBase Base64URLUnpadded

from64URLT :: ByteArray x => T.Text -> Either T.Text x
from64URLT = first T.pack . convertFromBase Base64URLUnpadded . T.encodeUtf8

from64S :: ByteArray x => String -> Either String x
from64S = convertFromBase Base64 . BSC.pack

from64URLS :: ByteArray x => String -> Either String x
from64URLS = convertFromBase Base64URLUnpadded . BSC.pack

fromHexTL :: ByteArray x => TL.Text -> Either T.Text x
fromHexTL = fromHexT . TL.toStrict

showKeyHexT :: SecKey -> T.Text
showKeyHexT = showPubKeyHexT . derivePubKey

showKeyHexTL :: SecKey -> TL.Text
showKeyHexTL = showPubKeyHexTL . derivePubKey

showPubKeyHexT :: PubKey -> T.Text
showPubKeyHexT = toHexT @ByteString . exportPubKey True

showPubKeyHexTL :: PubKey -> TL.Text
showPubKeyHexTL = toHexTL @ByteString . exportPubKey True
