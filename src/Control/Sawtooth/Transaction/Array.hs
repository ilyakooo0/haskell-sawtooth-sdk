{-# LANGUAGE PartialTypeSignatures #-}

module Control.Sawtooth.Transaction.Array
  ( Array,
    getLength,
    pushArray,
    popArray,
  )
where

import qualified Codec.Serialise as S
import Control.Monad.Except
import Control.Monad.Freer.Logger
import Control.Sawtooth.Transaction
import qualified Data.ByteString.Lazy as BSL
import Data.Kind
import Data.Sized
import Data.Text (Text)
import GHC.TypeLits
import Numeric.Natural.Sized

data Array (keySize :: Nat) (a :: Type)

instance (n <= 8, KnownNat n, Serialise a) => ComplexData (Array n a) where
  type ComplexDataKey (Array n _) = SizedNat n
  type ComplexDataValue (Array _ a) = a
  filterComplex n _ | n == maxBound = Nothing
  filterComplex _ b = Just . either (Left . show') Right . S.deserialiseOrFail $ BSL.fromStrict b

type instance TypeSize (Array n _) = n

getLength ::
  forall s n a m.
  (RWStateCplx s (Array n a) m, KnownNat n) =>
  m (SizedNat n)
getLength = do
  getStateCplx @s maxBound >>= \case
    Just x -> return x
    Nothing -> do
      setStateCplx @s maxBound (zeroNat @n)
      return zeroNat

setLength ::
  forall s n a m.
  (RWStateCplx s (Array n a) m, KnownNat n, MonadError Text m) =>
  SizedNat n ->
  m ()
setLength n = do
  assert (n < maxBound) "Length bounds exceeded"
  setStateCplx @s maxBound n

sizedNat' :: forall n m. (KnownNat n, MonadError Text m) => Natural -> m (SizedNat n)
sizedNat' = maybe (throwError "SizedNat limit exceeded.") return . sizedNat

-- liftSizedNat' :: forall n t. KnownNat n => (Natural -> Natural -> Natural) -> SizedNat n -> SizedNat n -> TransactionM t (SizedNat n)
-- liftSizedNat' f x y = maybe (throwError "SizedNat limit exceeded.") return $ liftSizedNat f x y

fmapSizedNat' :: forall n m. (KnownNat n, MonadError Text m) => (Natural -> Natural) -> SizedNat n -> m (SizedNat n)
fmapSizedNat' f x = maybe (throwError "SizedNat limit exceeded.") return $ fmapSizedNat f x

incLength ::
  forall s n a m.
  (RWStateCplx s (Array n a) m, KnownNat n, MonadError Text m) =>
  m (SizedNat n, SizedNat n)
incLength = do
  getStateCplx @s maxBound >>= \case
    Just x -> do
      x' <- fmapSizedNat' (+ 1) x
      setLength @s x'
      return (x, x')
    Nothing -> do
      x' <- sizedNat' @n 1
      setLength @s x'
      return (zeroNat, x')

decLength ::
  forall s n a m.
  (RWStateCplx s (Array n a) m, KnownNat n, MonadError Text m) =>
  m (SizedNat n, SizedNat n)
decLength = do
  getStateCplx @s maxBound >>= \case
    Just x | unSizedNat x > 0 -> do
      x' <- fmapSizedNat' (`subtract` 1) x
      setLength @s x'
      return (x, x')
    _ -> throwError "Can not decrement length of empty array."

pushArray ::
  forall s n a x m.
  (Serialise x, RWStateCplx s (Array n a) m, KnownNat n, MonadError Text m) =>
  x ->
  m ()
pushArray x = do
  (l, _) <- incLength @s
  setStateCplx @s l x

popArray ::
  forall s n a m.
  (RWStateCplx s (Array n a) m, KnownNat n, MonadError Text m) =>
  m ()
popArray = do
  (_, l) <- decLength @s
  deleteStateCplx @s l
