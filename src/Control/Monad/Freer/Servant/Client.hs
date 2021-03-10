{-# OPTIONS_GHC -Wno-orphans #-}

module Control.Monad.Freer.Servant.Client
  ( runServantClientM,
    ServantClient,
  )
where

import Control.Monad
import Control.Monad.Freer
import Control.Monad.IO.Class
import Network.HTTP.Types
import Servant.Client
import Servant.Client.Core

data ServantClient k where
  RunServantRequest :: Maybe [Status] -> Request -> ServantClient Response
  ThrowServant :: ClientError -> ServantClient a

instance Member ServantClient eff => RunClient (Eff eff) where
  runRequestAcceptStatus statuses req = send $ RunServantRequest statuses req
  throwClientError = send . ThrowServant

runServantClientM :: forall eff. MonadIO (Eff eff) => ClientEnv -> (forall x. ClientError -> Eff eff x) -> Eff (ServantClient ': eff) ~> Eff eff
runServantClientM env hErr = interpret $ \case
  RunServantRequest sts req -> runClientM' $ runRequestAcceptStatus sts req
  ThrowServant err -> hErr err
  where
    runClientM' :: ClientM x -> Eff eff x
    runClientM' = either hErr return <=< (liftIO . flip runClientM env)
