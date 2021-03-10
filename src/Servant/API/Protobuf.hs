module Servant.API.Protobuf
  ( Protobuf,
  )
where

import Data.Bifunctor
import qualified Data.ByteString.Lazy as BSL
import Data.Proxy
import qualified Proto3.Suite as P
import Servant.API

data Protobuf

instance Accept Protobuf where
  contentType Proxy = contentType (Proxy @OctetStream)

instance P.Message x => MimeUnrender Protobuf x where
  mimeUnrender Proxy = first show . P.fromByteString . BSL.toStrict

instance P.Message x => MimeRender Protobuf x where
  mimeRender Proxy = P.toLazyByteString
