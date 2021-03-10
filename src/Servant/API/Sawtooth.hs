module Servant.API.Sawtooth
  ( api,
    BatchLink (..),
    API (..),
    BatchId (..),
    getBatchIdFromLink,
    DataWrapper (..),
    BatchState (..),
    BatchStatus (..),
    JSONNoEncoding,
    B64ByteString (..),
    StateAddress (..),
    getStateFiltered,
    APIKey (..),
    getSState,
  )
where

import Codec.Serialise (deserialiseOrFail)
import Control.Monad
import Control.Sawtooth.Transaction
import Control.Sawtooth.Types
import Data.Aeson
import qualified Data.Aeson as A
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding.Extra
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Sized as BSS
import Data.Char
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe
import Data.Proxy
import qualified Data.Sawtooth.Batch as S
import Data.Sized
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.TypeLits
import qualified Network.HTTP.Media as M
import Servant.API
import Servant.API.Generic
import Servant.API.Protobuf
import Servant.Client.Core
import Servant.Client.Generic

api :: Proxy (ToServantApi API)
api = genericApi (Proxy @API)

data API route = API
  { sendBatches ::
      route :- "batches"
        :> ReqBody '[Protobuf] S.BatchList
        :> Post '[JSON] BatchLink,
    getBatchStatuses ::
      route :- "batch_statuses"
        :> ReqBody '[JSONNoEncoding] (NonEmpty BatchId)
        :> Post '[JSON] (DataWrapper (NonEmpty BatchState)),
    getSawtoothState ::
      route :- "state"
        :> QueryParam "address" APIKey
        :> QueryParam "start" StateAddress
        :> QueryParam "limit" Int
        :> QueryFlag "reverse"
        :> Get '[JSONNoEncoding] StatesResponse
  }
  deriving stock (Generic)

data APIKey = forall n t. KnownNat n => APIKey Namespace (Key' n t)

instance ToHttpApiData APIKey where
  toUrlPiece (APIKey n k) = TL.toStrict $ encodeKey' n k

getStateFiltered ::
  forall k m.
  ( RunClient m,
    ComplexData (ValueType k),
    KnownNat (ExtraKeySize (ValueType k)),
    Serialise k,
    KnownSymbol (TypeName k),
    Show k,
    (KnownNat (TotalSize - TypeSize (ValueType k)))
  ) =>
  Namespace ->
  k ->
  Maybe StateAddress ->
  Maybe Int ->
  Bool ->
  m (Either Text ([ComplexDataValue (ValueType k)], Maybe StateAddress))
getStateFiltered n k s p reversed = do
  res' <- getS (Just $ APIKey n (Key k :: Key' (TotalSize - TypeSize (ValueType k)) k)) s p reversed
  let res =
        mapMaybe
          ( \st -> do
              x <-
                either
                  (const Nothing)
                  ( decodeSized @(ComplexDataKey (ValueType k))
                      <=< BSS.byteString
                        . BS.drop (35 - fromInteger (natVal (Proxy @(ExtraKeySize (ValueType k)))))
                  )
                  . fromHexT
                  . unStateAddress
                  . address
                  $ (st :: State)
              filterComplex @(ValueType k) x (unB64ByteString $ datum (st :: State))
          )
          (datum (res' :: StatesResponse))
  return $ (,next_position . paging $ res') <$> sequence res
  where
    sawtoothAPI :: API (AsClientT m)
    sawtoothAPI = genericClient @API @m
    getS = getSawtoothState sawtoothAPI

getSState ::
  forall k m.
  ( RunClient m,
    Serialise k,
    KnownSymbol (TypeName k),
    Show k,
    Serialise (ValueType k)
  ) =>
  Namespace ->
  k ->
  m (Maybe (ValueType k))
getSState n k = do
  res' <- getS (Just $ APIKey n (Key k :: Key' TotalSize k)) Nothing Nothing False
  case datum (res' :: StatesResponse) of
    [x] ->
      return . either (const Nothing) Just $
        deserialiseOrFail . BSL.fromStrict . unB64ByteString $ datum (x :: State)
    _ -> return Nothing
  where
    sawtoothAPI :: API (AsClientT m)
    sawtoothAPI = genericClient @API @m
    getS = getSawtoothState sawtoothAPI

newtype BatchLink = BatchLink {link :: Text}
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (FromJSON, ToJSON)

newtype BatchId = BatchId {unBatchId :: Text}
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromJSON, ToJSON, ToHttpApiData, FromHttpApiData)

newtype StateAddress = StateAddress {unStateAddress :: Text}
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromJSON, ToJSON, ToHttpApiData, FromHttpApiData)

getBatchIdFromLink :: BatchLink -> BatchId
getBatchIdFromLink (BatchLink t) = BatchId $ T.takeWhileEnd (/= '=') t

data BatchStatus = BatchCommitted | BatchInvalid | BatchPending | BatchUnknown
  deriving stock (Generic, Eq, Ord, Show)

batchStatusJSONOptions :: Options
batchStatusJSONOptions =
  defaultOptions
    { constructorTagModifier = map toUpper . drop (length @[] "Batch")
    }

instance ToJSON BatchStatus where
  toJSON = genericToJSON batchStatusJSONOptions

instance FromJSON BatchStatus where
  parseJSON = genericParseJSON batchStatusJSONOptions

data BatchState = BatchState
  { id :: BatchId,
    status :: BatchStatus
  }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (FromJSON, ToJSON)

newtype DataWrapper a = DataWrapper {unDataWrapper :: a}

instance ToJSON a => ToJSON (DataWrapper a) where
  toJSON (DataWrapper x) = object ["data" .= toJSON x]

instance FromJSON a => FromJSON (DataWrapper a) where
  parseJSON = withObject "data wrapper" $ \o -> DataWrapper <$> o .: "data"

data JSONNoEncoding

instance Accept JSONNoEncoding where
  contentType = pure $ "application" M.// "json"

instance FromJSON a => MimeUnrender JSONNoEncoding a where
  mimeUnrender _ = mimeUnrender (Proxy @JSON)

instance ToJSON a => MimeRender JSONNoEncoding a where
  mimeRender _ = mimeRender (Proxy @JSON)

data StatesResponse = StatesResponse
  { datum :: [State],
    paging :: StatusPaging
  }
  deriving stock (Generic, Show, Eq, Ord)

instance ToJSON StatesResponse where
  toJSON = genericToJSON dataFieldJSONOptions

instance FromJSON StatesResponse where
  parseJSON = genericParseJSON dataFieldJSONOptions

data StatusPaging = StatusPaging
  { limit :: Maybe Int,
    next_position :: Maybe StateAddress
  }
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (FromJSON, ToJSON)

dataFieldJSONOptions :: Options
dataFieldJSONOptions =
  defaultOptions
    { fieldLabelModifier = \case
        "datum" -> "data"
        x -> x
    }

data State = State
  { address :: StateAddress,
    datum :: B64ByteString
  }
  deriving stock (Generic, Show, Eq, Ord)

instance ToJSON State where
  toJSON = genericToJSON dataFieldJSONOptions

instance FromJSON State where
  parseJSON = genericParseJSON dataFieldJSONOptions

newtype B64ByteString = B64ByteString {unB64ByteString :: BS.ByteString}
  deriving newtype (Show, Eq, Ord, BA.ByteArrayAccess)

instance ToJSON B64ByteString where
  toJSON (B64ByteString bs) = A.toJSON . to64T $ bs

instance FromJSON B64ByteString where
  parseJSON = A.parseJSON >=> either (fail . T.unpack) (return . B64ByteString) . from64T
