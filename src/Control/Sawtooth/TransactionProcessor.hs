module Control.Sawtooth.TransactionProcessor
  ( transactionMain,
    TPConfig (..),
  )
where

import Codec.Serialise
import Control.Concurrent
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Logger
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Time
import Control.Monad.IO.Class
import Control.Sawtooth.Transaction
import Control.Sawtooth.TransactionConstructor
import Control.Sawtooth.Types
import Crypto.Secp256k1
import Data.ByteArray.Encoding.Extra
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Kind
import qualified Data.Map as M
import Data.Proxy
import qualified Data.Sawtooth.Network as S
import qualified Data.Sawtooth.Processor as S
import qualified Data.Sawtooth.StateContext as S
import qualified Data.Sawtooth.Transaction as S
import qualified Data.Sawtooth.Validator as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import GHC.Conc
import GHC.TypeLits
import qualified Proto3.Suite as P
import Proto3.Wire.Decode
import System.ZMQ4.Extra
import System.ZMQ4.Monadic hiding (send)
import qualified System.ZMQ4.Monadic as MQ

transactionMain ::
  forall (ts :: [Type]).
  (ProcessTransaction ts, HasCallStack) =>
  TPConfig ->
  IO ()
transactionMain cfg = runZMQ $ do
  s <- socket Dealer
  connect s $ validatorAddress cfg
  corrMapT <- liftIO $ newTVarIO mempty
  let runLogger :: Eff '[Reader Namespace, Reader TransactionFamily, Logger (PrettyLog Text), TimeEff, ZMQ z] x -> ZMQ z x
      runLogger =
        runM
          . runTime
          . runTextLogger
          . prettifyLogs
          . runTFEff (tpTransactionFamily cfg)
      run' =
        runM
          . runTime
          . runTextLogger
          . prettifyLogs
          . flip (handleError @ParseError) (logErr . show')
          . flip (handleError @DeserialiseFailure) (logErr . show')
          . flip (handleError @Text) logErr
          . runReader s
          . runTFEff (tpTransactionFamily cfg)
          . runSawtoothRequest @Dealer corrMapT
      register =
        runLogger $ do
          tf <- ask
          namespace <- ask
          let req =
                S.TpRegisterRequest
                  { S.tpRegisterRequestFamily = tfName tf,
                    S.tpRegisterRequestVersion = tfVersion tf,
                    S.tpRegisterRequestNamespaces = pure . unNamespace $ namespace,
                    S.tpRegisterRequestMaxOccupancy = P.def,
                    S.tpRegisterRequestProtocolVersion = P.def,
                    S.tpRegisterRequestRequestHeaderStyle =
                      P.Enumerated $ Right S.TpRegisterRequest_TpProcessRequestHeaderStyleEXPANDED
                  }
          logInfo $ "Registering: " <> show' req
          sendM $
            MQ.send
              s
              []
              ( BSL.toStrict . P.toLazyByteString $
                  S.Message
                    { messageMessageType = P.Enumerated . Right $ requestType @S.TpRegisterRequest,
                      messageCorrelationId = P.def,
                      messageContent =
                        BSL.toStrict $ P.toLazyByteString req
                    }
              )
      unregister =
        runLogger $ do
          logInfo @Text "Unregistering"
          sendM $
            MQ.send
              s
              []
              ( BSL.toStrict . P.toLazyByteString $
                  S.Message
                    { messageMessageType = P.Enumerated . Right $ requestType @S.TpUnregisterRequest,
                      messageCorrelationId = P.def,
                      messageContent =
                        BSL.toStrict $ P.toLazyByteString S.TpUnregisterRequest
                    }
              )
  bracket register (const unregister) $ \_ ->
    forever $ do
      b <- receive s
      async
        . run'
        $ do
          m <- decodeMessage' @S.Message b
          logDebug . T.pack $ "Received message: " <> show m
          respMVar <- liftIO . atomically $ do
            corrMap <- readTVar corrMapT
            writeTVar corrMapT $ M.delete (S.messageCorrelationId m) corrMap
            return $ M.lookup (S.messageCorrelationId m) corrMap
          case respMVar of
            Just mvar -> liftIO $ putMVar mvar m
            Nothing ->
              do
                runSawtoothResponse @Dealer (S.messageCorrelationId m)
                . appendLogContext (TL.toStrict $ S.messageCorrelationId m)
                $ getEnumerated (S.messageMessageType m) >>= \case
                  S.Message_MessageTypePING_REQUEST -> sendResponse S.PingResponse
                  S.Message_MessageTypeTP_REGISTER_RESPONSE -> do
                    resp <- decodeMessage' @S.TpRegisterResponse (S.messageContent m)
                    logInfo $ show' resp
                  S.Message_MessageTypeTP_PROCESS_REQUEST -> do
                    req <- decodeMessage' @S.TpProcessRequest (S.messageContent m)
                    author <-
                      TransactionAuthor <$> case S.tpProcessRequestHeader req of
                        Nothing -> throwError ("Transaction header not present" :: Text)
                        Just h -> do
                          bs <-
                            either throwError return
                              . fromHexTL
                              $ S.transactionHeaderSignerPublicKey h
                          maybe (throwError @Text "Couldn't decode public key") return $ importPubKey bs
                    tData <- deserialise' @TransactionData . BSL.fromStrict $ S.tpProcessRequestPayload req
                    res <-
                      runError @Text . runError @ParseError . runError @DeserialiseFailure
                        . runSawtoothState (S.tpProcessRequestContextId req)
                        $ processTransaction @ts author tData
                    resp <- case res of
                      Right (Right (Right ())) ->
                        return
                          S.TpProcessResponse
                            { S.tpProcessResponseStatus = enumerated S.TpProcessResponse_StatusOK,
                              S.tpProcessResponseMessage = P.def,
                              S.tpProcessResponseExtendedData = P.def
                            }
                      Left x -> do
                        logErr $ show' x
                        return
                          S.TpProcessResponse
                            { S.tpProcessResponseStatus = enumerated S.TpProcessResponse_StatusINVALID_TRANSACTION,
                              S.tpProcessResponseMessage = TL.fromStrict x,
                              S.tpProcessResponseExtendedData = P.def
                            }
                      Right x -> do
                        logErr $ show' x
                        return
                          S.TpProcessResponse
                            { S.tpProcessResponseStatus = enumerated S.TpProcessResponse_StatusINTERNAL_ERROR,
                              S.tpProcessResponseMessage = P.def,
                              S.tpProcessResponseExtendedData = P.def
                            }
                    sendResponse resp
                  x -> logWarn $ "Unprocessed message of type " <> show' x

data TPConfig = TPConfig
  { tpTransactionFamily :: TransactionFamily,
    validatorAddress :: String
  }

decodeMessage' ::
  forall a eff.
  (Member (Error ParseError) eff, P.Message a) =>
  ByteString ->
  Eff eff a
decodeMessage' b = case P.fromByteString b of
  Right x -> return x
  Left err -> throwError err

deserialise' ::
  forall a eff.
  (Member (Error DeserialiseFailure) eff, Serialise a) =>
  BSL.ByteString ->
  Eff eff a
deserialise' b = case deserialiseOrFail b of
  Right x -> return x
  Left err -> throwError err

class ProcessTransaction (ts :: [Type]) where
  processTransaction ::
    (Members '[Error Text, Error DeserialiseFailure, SawtoothState] eff, LoggerEff eff) =>
    TransactionAuthor ->
    TransactionData ->
    Eff eff ()

instance ProcessTransaction '[] where
  processTransaction _ x =
    throwError $ "Unsupported transaction type: " <> transactionType x

instance
  forall xs x.
  (Transaction x, ProcessTransaction xs) =>
  ProcessTransaction (x ': xs)
  where
  processTransaction author x =
    if transactionType x == T.pack (symbolVal (Proxy @(TypeName x)))
      then do
        logDebug $ "Processing transaction: " <> transactionType x
        t <- deserialise' @x $ transactionData x
        logDebug $ "Deserialized transaction: " <> show' t
        runTransactionM
          (performTransaction t)
          (cells author t)
          TransactionRunner
            { getSt = \k -> do
                logDebug $ "Getting state: " <> show' k
                send . ReadState $ k,
              setSt = \k u -> do
                logDebug $ "Setting state: " <> show' k
                send $ SetState k u,
              deleteSt = \k -> do
                logDebug $ "Deleting state: " <> show' k
                send . DeleteState $ k,
              err = throwError,
              ctch = catchError,
              athr = author
            }
      else processTransaction @xs author x

-- NOTE: for simple type `y` and `x` should be the same. They are distinct here to
-- support complex types.
data SawtoothState x where
  SetState :: Serialise x => Key y -> x -> SawtoothState ()
  ReadState :: Serialise x => Key y -> SawtoothState (Maybe x)
  DeleteState :: Key x -> SawtoothState ()

runSawtoothState ::
  ( Members
      '[ Error DeserialiseFailure,
         Error Text,
         SendSawtoothRequest,
         Reader Namespace
       ]
      eff,
    LoggerEff eff
  ) =>
  TL.Text ->
  Eff (SawtoothState ': eff) ~> Eff eff
runSawtoothState ctx = interpret $ \case
  ReadState key -> do
    encKey <- encodeKey key
    logDebug $ "Getting with key: " <> show' encKey
    resp <-
      sendRequest
        S.TpStateGetRequest
          { S.tpStateGetRequestContextId = ctx,
            S.tpStateGetRequestAddresses = pure encKey
          }
    unless
      (S.tpStateGetResponseStatus resp == enumerated S.TpStateGetResponse_StatusOK)
      (throwError @Text "Request failed")
    case V.toList (S.tpStateGetResponseEntries resp) of
      [x] | (BS.null . S.tpStateEntryData) x -> return Nothing
      [x] -> fmap Just . deserialise' . BSL.fromStrict $ S.tpStateEntryData x
      _ -> throwError @Text "Wrong amount of entries"
  SetState key val -> do
    encKey <- encodeKey key
    logDebug $ "Setting with key: " <> show' encKey
    resp <-
      sendRequest
        S.TpStateSetRequest
          { S.tpStateSetRequestContextId = ctx,
            S.tpStateSetRequestEntries =
              pure
                S.TpStateEntry
                  { S.tpStateEntryAddress = encKey,
                    S.tpStateEntryData = BSL.toStrict $ serialise val
                  }
          }
    unless
      (S.tpStateSetResponseStatus resp == enumerated S.TpStateSetResponse_StatusOK)
      (throwError @Text "Request failed")
  DeleteState key -> do
    encKey <- encodeKey key
    logDebug $ "Deleting with key: " <> show' encKey
    resp <-
      sendRequest
        S.TpStateDeleteRequest
          { S.tpStateDeleteRequestContextId = ctx,
            S.tpStateDeleteRequestAddresses = pure encKey
          }
    unless
      (S.tpStateDeleteResponseStatus resp == enumerated S.TpStateDeleteResponse_StatusOK)
      (throwError @Text "Request failed")

enumerated :: a -> P.Enumerated a
enumerated = P.Enumerated . Right
