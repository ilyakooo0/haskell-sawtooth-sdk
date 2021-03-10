module Control.Sawtooth.TransactionConstructor
  ( derivePubKey,
    runTransactionConstructor,
    TransactionConstructor (..),
    generateIdentity,
    TransactionData (..),
    encodeKey,
    encodeKey',
    encodeTransactionBatch,
    SomeTransaction (..),
    EncodableTransaction,
    sendTransactions,
    sendTransaction,
    BatcherSecKey (..),
    RequestDumper (..),
    dumpRequestsToFile,
    noDumpRequests,
  )
where

import Codec.Serialise
import Control.Monad.Freer
import Control.Monad.Freer.Logger hiding (msg)
import Control.Monad.Freer.Random
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Servant.Client
import Control.Monad.Freer.Time
import Control.Monad.IO.Class
import Control.Sawtooth.Transaction
import Control.Sawtooth.Types
import Crypto.Hash
import Crypto.Secp256k1
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding.Extra
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Proxy
import Data.Sawtooth.Batch
import qualified Data.Sawtooth.Batch as S
import qualified Data.Sawtooth.Transaction as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time.Format.ISO8601
import qualified Data.Type.Map as TM
import qualified Data.Vector as V
import GHC.Generics
import GHC.TypeLits
import qualified Proto3.Suite as P
import qualified Servant.API.Sawtooth as SAPI
import Servant.Client.Generic
import qualified System.Envy as E

data TransactionConstructor k where
  GenerateIdentity :: TransactionConstructor SecKey
  SendTransactions :: NonEmpty SomeTransaction -> TransactionConstructor SAPI.BatchLink

generateIdentity :: Member TransactionConstructor eff => Eff eff SecKey
generateIdentity = send GenerateIdentity

sendTransactions :: Member TransactionConstructor eff => NonEmpty SomeTransaction -> Eff eff SAPI.BatchLink
sendTransactions ts = send $ SendTransactions ts

sendTransaction :: (Member TransactionConstructor eff, EncodableTransaction t) => SecKey -> t -> Eff eff SAPI.BatchLink
sendTransaction sec t = send $ SendTransactions (pure $ SomeTransaction sec t)

data RequestDumper x where
  DumpRequest :: BatchList -> RequestDumper ()

dumpRequestsToFile ::
  (Members '[TimeEff] eff, MonadIO (Eff eff)) =>
  FilePath ->
  Eff (RequestDumper ': eff) ~> Eff eff
dumpRequestsToFile path = interpret $ \(DumpRequest batch) -> do
  time <- currentTime
  liftIO $ BSL.writeFile (path <> "/" <> iso8601Show time <> ".bin") (P.toLazyByteString batch)

noDumpRequests ::
  Eff (RequestDumper ': eff) ~> Eff eff
noDumpRequests = interpret $ \(DumpRequest _) -> pure ()

runTransactionConstructor ::
  forall eff x.
  ( Members
      '[ Random,
         ServantClient,
         Reader Namespace,
         Reader TransactionFamily,
         Reader BatcherSecKey,
         TimeEff,
         Logger (PrettyLog Text),
         RequestDumper
       ]
      eff
  ) =>
  Eff (TransactionConstructor ': eff) x ->
  Eff eff x
runTransactionConstructor = interpret $ \case
  GenerateIdentity -> do
    bs <- random 32
    return . fromJust $ secKey bs
  SendTransactions ts -> do
    batch <- encodeTransactionBatch ts
    send $ DumpRequest batch
    sendBatches batch
    where
      sawtoothAPI :: SAPI.API (AsClientT (Eff eff))
      sawtoothAPI = genericClient @SAPI.API @(Eff eff)
      sendBatches = SAPI.sendBatches sawtoothAPI

encodeTransaction :: forall x. Transaction x => x -> BSL.ByteString
encodeTransaction t =
  serialise
    TransactionData
      { transactionType = T.pack $ symbolVal (Proxy @(TypeName x)),
        transactionData = serialise t
      }

-- | 'sec' is the user private key.
encodeSawtoothTransaction ::
  forall t effs.
  ( Members
      '[ Reader Namespace,
         Reader TransactionFamily,
         Random,
         Reader BatcherSecKey,
         TimeEff,
         Logger (PrettyLog Text)
       ]
      effs,
    EncodableTransaction t
  ) =>
  SecKey ->
  t ->
  Eff effs S.Transaction
encodeSawtoothTransaction sec t = do
  n <- ask
  tf <- ask
  nonce <- toHexTL @ByteString <$> random 32
  (BatcherSecKey bSec) <- ask
  let author = TransactionAuthor . derivePubKey $ sec
      header =
        S.TransactionHeader
          { S.transactionHeaderBatcherPublicKey = showKeyHexTL bSec,
            S.transactionHeaderDependencies = V.empty,
            S.transactionHeaderFamilyName = tfName tf,
            S.transactionHeaderFamilyVersion = tfVersion tf,
            S.transactionHeaderNonce = nonce,
            S.transactionHeaderInputs = V.fromList $ encodeKeysMap n $ inputs @t author t,
            S.transactionHeaderOutputs = V.fromList $ encodeKeysMap n $ outputs @t author t,
            S.transactionHeaderPayloadSha512 = toHexTL $ hashWith SHA512 tr,
            S.transactionHeaderSignerPublicKey = showKeyHexTL sec
          }
      hdr = BSL.toStrict $ P.toLazyByteString header
      tr = BSL.toStrict $ encodeTransaction t
  logDebug . show' $ t
  logDebug . show' $ header
  return
    S.Transaction
      { S.transactionPayload = tr,
        S.transactionHeader = hdr,
        S.transactionHeaderSignature = toHexTL $ getCompactSignature sec hdr
      }

encodeTransactionBatch ::
  forall effs.
  ( Members
      '[ Reader Namespace,
         Reader TransactionFamily,
         Reader BatcherSecKey,
         Random,
         TimeEff,
         Logger (PrettyLog Text)
       ]
      effs
  ) =>
  NonEmpty SomeTransaction ->
  Eff effs S.BatchList
encodeTransactionBatch ts = do
  ts' <- traverse (\(SomeTransaction sec t) -> encodeSawtoothTransaction sec t) ts
  (BatcherSecKey bSec) <- ask
  let header =
        S.BatchHeader
          { S.batchHeaderSignerPublicKey = showKeyHexTL bSec,
            S.batchHeaderTransactionIds =
              V.fromList . NE.toList $ S.transactionHeaderSignature <$> ts'
          }
      hdr = BSL.toStrict $ P.toLazyByteString header
  return
    S.BatchList
      { S.batchListBatches =
          V.singleton
            S.Batch
              { S.batchHeader = hdr,
                S.batchHeaderSignature = toHexTL $ getCompactSignature bSec hdr,
                S.batchTransactions = V.fromList . NE.toList $ ts',
                S.batchTrace = False
              }
      }

newtype BatcherSecKey = BatcherSecKey SecKey

instance E.Var BatcherSecKey where
  toVar (BatcherSecKey sec) = to64S . getSecKey $ sec
  fromVar = fmap BatcherSecKey . (>>= secKey) . either (const Nothing) Just . from64S

data SomeTransaction where
  SomeTransaction ::
    EncodableTransaction t =>
    SecKey ->
    t ->
    SomeTransaction

type EncodableTransaction t =
  ( Transaction t,
    EncodeKeysMap (InputsMap t),
    EncodeKeysMap (OutputsMap t)
  )

getCompactSignature :: BA.ByteArrayAccess ba => SecKey -> ba -> ByteString
getCompactSignature sec b =
  getCompactSig . exportCompactSig . signMsg sec . fromJust . msg . BS.pack . BA.unpack $ hashWith SHA256 b

data TransactionData = TransactionData
  { transactionType :: Text,
    transactionData :: BSL.ByteString
  }
  deriving stock (Generic, Eq, Ord, Show)
  deriving anyclass (Serialise)

class EncodeKeysMap (ds :: [Declaration]) where
  encodeKeysMap :: Namespace -> TM.Map ds -> [TL.Text]

instance EncodeKeysMap '[] where
  encodeKeysMap _ Empty = []

instance (EncodeKeysMap ds, KnownNat n) => EncodeKeysMap ((u ':-> Key' n t) ': ds) where
  encodeKeysMap n (Ext _ k rest) = encodeKey' n k : encodeKeysMap n rest
