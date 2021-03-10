module Control.Monad.Freer.Logger
  ( Severity (..),
    Logger (..),
    PrettyLog (..),
    logWarn,
    logInfo,
    logDebug,
    logErr,
    prettifyLogs,
    runTextLogger,
    HasCallStack,
    show',
    appendLogContext,
    LoggerEff,
  )
where

import Colourista.Pure
import Control.Monad.Freer
import Control.Monad.Freer.Time
import Control.Monad.IO.Class
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import GHC.Stack
import System.IO
import Prelude hiding (log)

data Severity = Debug | Info | Warn | Err
  deriving stock (Eq, Ord, Show)

showSeverity :: Severity -> Text
showSeverity Debug = formatWith [underline] "Debug"
showSeverity Info = formatWith [underline] "Info"
showSeverity Warn = formatWith [yellow, underline] "Warning"
showSeverity Err = formatWith [red, underline] "Error"

showTime :: UTCTime -> Text
showTime =
  T.pack . formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%Ez"))

data Logger s x where
  Log :: s -> Logger s ()

data PrettyLog s = PrettyLog
  { msg :: s,
    severity :: Severity,
    logContext :: [Text]
  }

log :: Member (Logger s) eff => s -> Eff eff ()
log = send . Log

logPretty ::
  (Members '[Logger (PrettyLog s), TimeEff] eff, HasCallStack) =>
  Severity ->
  s ->
  Eff eff ()
logPretty sev s = withFrozenCallStack $ do
  time <- currentTime
  send $
    Log
      PrettyLog
        { msg = s,
          severity = sev,
          logContext = [compactCallStack callStack, showTime time]
        }

logDebug ::
  (Members '[Logger (PrettyLog s), TimeEff] eff, HasCallStack) =>
  s ->
  Eff eff ()
logDebug t = withFrozenCallStack $ logPretty Debug t

logInfo ::
  (Members '[Logger (PrettyLog s), TimeEff] eff, HasCallStack) =>
  s ->
  Eff eff ()
logInfo t = withFrozenCallStack $ logPretty Info t

logWarn ::
  (Members '[Logger (PrettyLog s), TimeEff] eff, HasCallStack) =>
  s ->
  Eff eff ()
logWarn t = withFrozenCallStack $ logPretty Warn t

logErr ::
  (Members '[Logger (PrettyLog s), TimeEff] eff, HasCallStack) =>
  s ->
  Eff eff ()
logErr t = withFrozenCallStack $ logPretty Err t

runTextLogger :: MonadIO (Eff effs) => Eff (Logger Text ': effs) a -> Eff effs a
runTextLogger = interpret $ \(Log s) -> liftIO $ do
  T.putStrLn s
  hFlush stdout

prettifyLogs :: Eff (Logger (PrettyLog Text) ': effs) a -> Eff (Logger Text ': effs) a
prettifyLogs =
  reinterpret
    ( \(Log PrettyLog {..}) ->
        log $ bracketize (showSeverity severity : reverse logContext) <> " " <> formatWith [bold] msg
    )

appendLogContext :: Member (Logger (PrettyLog Text)) eff => Text -> Eff (Logger (PrettyLog Text) ': eff) ~> Eff eff
appendLogContext ctx = interpret (\(Log s) -> log s {logContext = ctx : logContext s})

compactCallStack :: CallStack -> Text
compactCallStack =
  T.pack
    . unwords
    . List.intersperse "->"
    . map (\(f, l) -> f <> " (" <> compactLoc l <> ")")
    . getCallStack
  where
    compactLoc loc = srcLocFile loc <> ":" <> show (srcLocStartLine loc)

bracketize :: [Text] -> Text
bracketize = foldMap $ (<> "]") . ("[" <>)

show' :: Show a => a -> Text
show' = T.pack . show

type LoggerEff eff =
  Members
    '[ TimeEff,
       Logger (PrettyLog Text)
     ]
    eff
