module System.ZMQ4.Extra
  ( receiveP,
    parseMessage,
    getEnumerated,
    SendSawtoothResponse (..),
    SendSawtoothRequest (..),
    SawtoothResponse (..),
    SawtoothRequest (..),
    sendResponse,
    runSawtoothResponse,
    runSawtoothRequest,
    sendRequest,
  )
where

import Control.Concurrent
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Logger
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Time
import Control.Monad.IO.Class
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Kind
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Sawtooth.Network as S
import qualified Data.Sawtooth.Processor as S
import qualified Data.Sawtooth.StateContext as S
import qualified Data.Sawtooth.Validator as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import GHC.Conc
import qualified Proto3.Suite as P
import Proto3.Wire.Decode
import qualified System.ZMQ4.Monadic as MQ

receiveP ::
  forall x z a effs.
  (P.Message x, LastMember (MQ.ZMQ z) effs, Members '[Error ParseError] effs, MQ.Receiver a) =>
  MQ.Socket z a ->
  Eff effs x
receiveP s = do
  b <- sendM $ MQ.receive s
  parseMessage @x b

parseMessage ::
  forall x effs.
  (P.Message x, Members '[Error ParseError] effs) =>
  BS.ByteString ->
  Eff effs x
parseMessage b = do
  case P.fromByteString @x b of
    Left err -> throwError err
    Right x -> return x

getEnumerated :: Member (Error Text) eff => P.Enumerated a -> Eff eff a
getEnumerated (P.Enumerated (Right x)) = return x
getEnumerated (P.Enumerated (Left x)) = throwError $ "Unknown enum " <> show' x

class P.Message x => SawtoothResponse x where
  responseType :: S.Message_MessageType

class (SawtoothResponse (ResponseType x), P.Message x) => SawtoothRequest x where
  type ResponseType x :: Type
  requestType :: S.Message_MessageType

instance SawtoothResponse S.PingResponse where
  responseType = S.Message_MessageTypePING_RESPONSE

instance SawtoothResponse S.TpRegisterResponse where
  responseType = S.Message_MessageTypeTP_REGISTER_RESPONSE

instance SawtoothRequest S.TpRegisterRequest where
  type ResponseType S.TpRegisterRequest = S.TpRegisterResponse
  requestType = S.Message_MessageTypeTP_REGISTER_REQUEST

instance SawtoothResponse S.TpProcessResponse where
  responseType = S.Message_MessageTypeTP_PROCESS_RESPONSE

instance SawtoothResponse S.TpStateGetResponse where
  responseType = S.Message_MessageTypeTP_STATE_GET_RESPONSE

instance SawtoothRequest S.TpStateGetRequest where
  type ResponseType S.TpStateGetRequest = S.TpStateGetResponse
  requestType = S.Message_MessageTypeTP_STATE_GET_REQUEST

instance SawtoothResponse S.TpStateSetResponse where
  responseType = S.Message_MessageTypeTP_STATE_SET_RESPONSE

instance SawtoothRequest S.TpStateSetRequest where
  type ResponseType S.TpStateSetRequest = S.TpStateSetResponse
  requestType = S.Message_MessageTypeTP_STATE_SET_REQUEST

instance SawtoothResponse S.TpStateDeleteResponse where
  responseType = S.Message_MessageTypeTP_STATE_DELETE_RESPONSE

instance SawtoothRequest S.TpStateDeleteRequest where
  type ResponseType S.TpStateDeleteRequest = S.TpStateDeleteResponse
  requestType = S.Message_MessageTypeTP_STATE_DELETE_REQUEST

instance SawtoothResponse S.TpUnregisterResponse where
  responseType = S.Message_MessageTypeTP_UNREGISTER_RESPONSE

instance SawtoothRequest S.TpUnregisterRequest where
  type ResponseType S.TpUnregisterRequest = S.TpUnregisterResponse
  requestType = S.Message_MessageTypeTP_UNREGISTER_REQUEST

sendResponse ::
  (SawtoothResponse t, Show t, Member SendSawtoothResponse effs) =>
  t ->
  Eff effs ()
sendResponse = send . SendSawtoothResponse

sendRequest ::
  (SawtoothRequest t, Show t, Member SendSawtoothRequest effs) =>
  t ->
  Eff effs (ResponseType t)
sendRequest = send . SendSawtoothRequest

data SendSawtoothResponse x where
  SendSawtoothResponse :: (SawtoothResponse t, Show t) => t -> SendSawtoothResponse ()

data SendSawtoothRequest x where
  SendSawtoothRequest :: (SawtoothRequest t, Show t) => t -> SendSawtoothRequest (ResponseType t)

runSawtoothRequest ::
  forall r z effs x.
  ( LastMember (MQ.ZMQ z) effs,
    MQ.Sender r,
    Members
      '[ Reader (MQ.Socket z r),
         Logger (PrettyLog Text),
         Error Text,
         TimeEff,
         Error ParseError
       ]
      effs,
    MonadIO (Eff effs)
  ) =>
  TVar (Map TL.Text (MVar S.Message)) ->
  Eff (SendSawtoothRequest ': effs) x ->
  Eff effs x
runSawtoothRequest map' = interpret $ \(SendSawtoothRequest (x :: t)) -> do
  uuid <- TL.fromStrict . UUID.toText <$> liftIO UUID.nextRandom
  mvar <- liftIO newEmptyMVar
  liftIO . atomically $ do
    m <- readTVar map'
    writeTVar map' $ M.insert uuid mvar m
  s <- ask @(MQ.Socket z r)
  sendM $
    MQ.send
      s
      []
      ( BSL.toStrict . P.toLazyByteString $
          S.Message
            { messageMessageType = P.Enumerated . Right $ requestType @t,
              messageCorrelationId = uuid,
              messageContent = BSL.toStrict $ P.toLazyByteString x
            }
      )
  logDebug $ "Sent request: " <> TL.toStrict uuid <> " " <> show' x
  resp <- liftIO $ takeMVar mvar
  if S.messageMessageType resp == P.Enumerated (Right (responseType @(ResponseType t)))
    then parseMessage $ S.messageContent resp
    else
      throwError $
        "Expected response " <> show' (responseType @(ResponseType t))
          <> " but got "
          <> show' (S.messageMessageType resp)

runSawtoothResponse ::
  forall r z effs x.
  (LastMember (MQ.ZMQ z) effs, MQ.Sender r, Members '[Reader (MQ.Socket z r), Logger (PrettyLog Text), TimeEff] effs) =>
  TL.Text ->
  Eff (SendSawtoothResponse ': effs) x ->
  Eff effs x
runSawtoothResponse corrId = interpret $ \(SendSawtoothResponse (x :: t)) -> do
  logDebug . ("Sent message: " <>) . T.pack $ show x
  s <- ask @(MQ.Socket z r)
  sendM $
    MQ.send
      s
      []
      ( BSL.toStrict . P.toLazyByteString $
          S.Message
            { messageMessageType = P.Enumerated . Right $ responseType @t,
              messageCorrelationId = corrId,
              messageContent = BSL.toStrict $ P.toLazyByteString x
            }
      )
