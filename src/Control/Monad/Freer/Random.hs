module Control.Monad.Freer.Random
  ( Random (..),
    runRandom,
    random,
  )
where

import Control.Monad.Freer
import Control.Monad.IO.Class
import Crypto.Random
import Data.ByteArray

data Random k where
  RandomBytes :: ByteArray byteArray => Int -> Random byteArray

random :: Member Random eff => ByteArray byteArray => Int -> Eff eff byteArray
random = send . RandomBytes

runRandom :: MonadIO (Eff effs) => Eff (Random ': effs) x -> Eff effs x
runRandom = interpret $ \(RandomBytes i) -> liftIO $ getRandomBytes i
