module Control.Monad.Freer.Time
  ( TimeEff (..),
    runTime,
    currentTime,
  )
where

import Control.Monad.Freer
import Control.Monad.IO.Class
import Data.Time

data TimeEff x where
  GetCurrentTime :: TimeEff UTCTime

currentTime :: Member TimeEff eff => Eff eff UTCTime
currentTime = send GetCurrentTime

runTime :: MonadIO (Eff eff) => Eff (TimeEff ': eff) a -> Eff eff a
runTime = interpret $ \GetCurrentTime -> liftIO getCurrentTime
