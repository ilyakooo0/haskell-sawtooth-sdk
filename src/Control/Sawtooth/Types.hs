module Control.Sawtooth.Types
  ( Namespace (..),
    getNamespace,
    TransactionFamily (..),
    runTFEff,
  )
where

import Control.Monad.Freer
import Control.Monad.Freer.Reader
import Crypto.Hash
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding.Extra
import qualified Data.ByteString as BS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

newtype Namespace = Namespace {unNamespace :: TL.Text}

getNamespace :: TransactionFamily -> Namespace
getNamespace t =
  let digest :: Digest SHA256 = hashlazy . TL.encodeUtf8 . tfName $ t
   in Namespace . toHexTL . BS.pack . take 3 . BA.unpack $ digest

data TransactionFamily = TransactionFamily {tfName :: TL.Text, tfVersion :: TL.Text}

runTFEff ::
  TransactionFamily ->
  Eff (Reader Namespace ': Reader TransactionFamily ': effs) ~> Eff effs
runTFEff tf = runReader tf . runReader (getNamespace tf)
