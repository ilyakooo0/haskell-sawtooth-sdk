module Control.Sawtooth.Transaction
  ( Transaction (..),
    Key' (..),
    Key,
    encodeKey,
    encodeKey',
    SimpleKey (..),
    pattern Key,
    TypeName,
    TransactionAuthor (..),
    Declaration,
    TransactionM (..),
    TransactionRunner (..),
    RState (..),
    WState (..),
    RWState,
    getStateCplx,
    WStateCplx,
    RStateCplx,
    setStateCplx,
    deleteStateCplx,
    pattern Empty,
    pattern Ext,
    asMap,
    Var (..),
    (&),
    inputs,
    outputs,
    InputsMap,
    OutputsMap,
    Serialise (..),
    Mapping (..),
    type (::->),
    ComplexData (..),
    RWStateCplx,
    assert,
    assertStateEmpty,
    getStateFull,
    modifyState,
    modifyStateFull,
    modifyStateFullM,
    ExtraKeySize,
    TotalSize,
    ValueType,
  )
where

import Codec.Serialise
import Control.Monad
import Control.Monad.Except
import Control.Monad.Freer
import qualified Control.Monad.Freer.Reader as Eff
import Control.Monad.Reader
import Control.Sawtooth.Types
import Crypto.Hash
import Crypto.Secp256k1
import qualified Data.ByteArray as BA
import qualified Data.ByteArray as BS
import Data.ByteArray.Encoding.Extra
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Sized as BSS
import Data.Function
import Data.Kind
import Data.Proxy
import Data.Sized
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Type.Map (Map (..), Mapping (..), Var (..), asMap)
import qualified Data.Type.Map as TM
import Data.Type.Set (Sort, (:++))
import GHC.Generics (Generic)
import GHC.TypeLits

type Declaration = TM.Mapping Symbol Type

class
  ( Serialise t,
    KnownSymbol (TypeName t),
    TM.Submap (InputsMap t) (AllDeclKeys t),
    TM.Submap (OutputsMap t) (AllDeclKeys t),
    Show t
  ) =>
  Transaction t
  where
  type Inputs t :: [Declaration]
  type Outputs t :: [Declaration]
  cells :: TransactionAuthor -> t -> TM.Map (AllDeclKeys t)
  performTransaction :: t -> TransactionM t ()

newtype TransactionAuthor = TransactionAuthor {unTransactionAuthor :: PubKey}
  deriving newtype (Eq, Show)

type Union m n = Nub (Sort (m :++ n))

type family Nub t where
  Nub '[] = '[]
  Nub '[kvp] = '[kvp]
  Nub ((k ':-> v) ': (k ':-> v) ': m) = Nub ((k ':-> v) ': m)
  Nub (kvp1 ': kvp2 ': s) = kvp1 ': Nub (kvp2 ': s)

type AllDeclKeys t = KeyTheMap (Union (Inputs t) (Outputs t))

type InputsMap t = KeyTheMap (TM.AsMap (Inputs t))

type OutputsMap t = KeyTheMap (TM.AsMap (Outputs t))

type family KeyTheMap (xs :: [Declaration]) :: [Declaration] where
  KeyTheMap '[] = '[]
  KeyTheMap ((s ':-> ComplexK t) ': xs) = (s ':-> Key' (TotalSize - TypeSize (ValueType t)) t) ': KeyTheMap xs
  KeyTheMap ((s ':-> t) ': xs) = (s ':-> Key t) ': KeyTheMap xs

type family TypeName t = (s :: Symbol) | s -> t

type family ValueType k = v

type Key x = Key' TotalSize x

data Key' (n :: Nat) t where
  SKey :: SimpleKey t -> Key' n t
  CKey :: forall (n :: Nat) s t. (KnownNat n, SizedType s, Show s) => Key' n t -> s -> Key' (n + TypeSize s) t

deriving stock instance Show (Key' (n :: Nat) t)

pattern Key ::
  forall k n.
  (Serialise k, KnownSymbol (TypeName k), Show k) =>
  k ->
  Key' n k
pattern Key k = SKey (SimpleKey k)

data SimpleKey (t :: Type) where
  SimpleKey ::
    forall k.
    (Serialise k, KnownSymbol (TypeName k), Show k) =>
    k ->
    SimpleKey k

deriving stock instance Show (SimpleKey (t :: Type))

newtype TransactionM t a = TransactionM
  { runTransactionM ::
      forall m.
      Monad m =>
      TM.Map (AllDeclKeys t) ->
      TransactionRunner m ->
      m a
  }

-- NOTE: for simple type `y` and `x` should be the same. They are distinct here to
-- support complex types.
data TransactionRunner m = TransactionRunner
  { getSt :: forall y x. Serialise x => Key y -> m (Maybe x),
    setSt :: forall y x. Serialise x => Key y -> x -> m (),
    deleteSt :: forall y. Key y -> m (),
    err :: forall x. Text -> m x,
    ctch :: forall x. m x -> (Text -> m x) -> m x,
    athr :: TransactionAuthor
  }

instance Functor (TransactionM t) where
  fmap f (TransactionM m) = TransactionM $ \a b -> f <$> m a b

instance Applicative (TransactionM t) where
  (TransactionM lhs) <*> (TransactionM rhs) = TransactionM $ \a b -> lhs a b <*> rhs a b
  pure x = TransactionM $ \_ _ -> return x

instance Monad (TransactionM t) where
  return = pure
  (TransactionM m) >>= f = TransactionM $
    \a b -> m a b >>= fmap (\u -> runTransactionM u a b) f

instance MonadError Text (TransactionM t) where
  throwError t = TransactionM $ \_ f -> err f t
  catchError (TransactionM m) h = TransactionM $ \ma f -> ctch f (m ma f) ((\(TransactionM u) -> u ma f) <$> h)

instance MonadReader TransactionAuthor (TransactionM tran) where
  ask = TransactionM $ \_ f -> return $ athr f
  local f (TransactionM h) = TransactionM $ \m u -> h m (u {athr = f $ athr u})

class Monad m => WState (s :: Symbol) (t :: Type) (m :: Type -> Type) | m s -> t where
  setState :: Serialise t => t -> m ()
  deleteState :: m ()

instance
  (TM.IsMember s (Key k) (OutputsMap tran), TM.Submap (OutputsMap tran) (AllDeclKeys tran), t ~ ValueType k) =>
  WState s t (TransactionM tran)
  where
  setState x = TransactionM $ \m f -> setSt f (TM.lookp (TM.Var @s) (TM.submap @(OutputsMap tran) m)) x
  deleteState = TransactionM $ \m f -> deleteSt f (TM.lookp (TM.Var @s) (TM.submap @(OutputsMap tran) m))

class Monad m => RState (s :: Symbol) (t :: Type) (m :: Type -> Type) | m s -> t where
  getState :: Serialise t => m (Maybe t)

instance
  (TM.IsMember s (Key k) (InputsMap tran), TM.Submap (InputsMap tran) (AllDeclKeys tran), t ~ ValueType k) =>
  RState s t (TransactionM tran)
  where
  getState = TransactionM $ \m f -> getSt f $ TM.lookp (TM.Var @s) (TM.submap @(InputsMap tran) m)

type RWState s t m = (RState s t m, WState s t m)

class (Monad m, ComplexData t) => WStateCplx (s :: Symbol) (t :: Type) (m :: Type -> Type) | m s -> t where
  setStateCplx :: Serialise x => ComplexDataKey t -> x -> m ()
  deleteStateCplx :: ComplexDataKey t -> m ()

instance
  ( TM.IsMember s (Key' (TotalSize - ExtraKeySize t) k) (OutputsMap tran),
    TM.Submap (OutputsMap tran) (AllDeclKeys tran),
    ComplexData t,
    KnownNat (TotalSize - ExtraKeySize t),
    Show (ComplexDataKey t),
    ((TotalSize - ExtraKeySize t) + ExtraKeySize t) ~ TotalSize,
    ValueType k ~ t
  ) =>
  WStateCplx s t (TransactionM tran)
  where
  setStateCplx extraKey x = TransactionM $ \m f ->
    setSt
      f
      ( CKey
          (TM.lookp (TM.Var @s) (TM.submap @(OutputsMap tran) m))
          extraKey
      )
      x
  deleteStateCplx extraKey = TransactionM $ \m f ->
    deleteSt
      f
      $ CKey
        (TM.lookp (TM.Var @s) (TM.submap @(OutputsMap tran) m))
        extraKey

class Monad m => RStateCplx (s :: Symbol) (t :: Type) (m :: Type -> Type) | m s -> t where
  getStateCplx :: Serialise x => ComplexDataKey t -> m (Maybe x)

instance
  ( TM.IsMember s (Key' (TotalSize - ExtraKeySize t) k) (InputsMap tran),
    TM.Submap (InputsMap tran) (AllDeclKeys tran),
    ComplexData t,
    KnownNat (TotalSize - ExtraKeySize t),
    ((TotalSize - ExtraKeySize t) + ExtraKeySize t) ~ TotalSize,
    t ~ ValueType k,
    Show (ComplexDataKey t)
  ) =>
  RStateCplx s t (TransactionM tran)
  where
  getStateCplx extra = TransactionM $ \m f ->
    getSt f $
      CKey
        (TM.lookp (TM.Var @s) (TM.submap @(InputsMap tran) m))
        extra

type RWStateCplx s t m = (RStateCplx s t m, WStateCplx s t m)

assert :: MonadError e m => Bool -> e -> m ()
assert c t = unless c $ throwError t

assertStateEmpty :: forall s e t m. (Serialise t, MonadError e m, RState s t m) => e -> m ()
assertStateEmpty e =
  getState @s >>= \case
    Nothing -> return ()
    Just _ -> throwError e

getStateFull :: forall s e t m. (Serialise t, MonadError e m, RState s t m) => e -> m t
getStateFull e =
  getState @s >>= \case
    Nothing -> throwError e
    Just t -> return t

modifyState :: forall s t m. (Serialise t, RWState s t m) => (Maybe t -> t) -> m ()
modifyState f = do
  x <- getState @s
  setState @s (f x)

modifyStateFull :: forall s e t m. (Serialise t, MonadError e m, RWState s t m) => e -> (t -> t) -> m ()
modifyStateFull e f = do
  x <- getStateFull @s e
  setState @s (f x)

modifyStateFullM :: forall s e t m. (Serialise t, MonadError e m, RWState s t m) => e -> (t -> m t) -> m ()
modifyStateFullM e f = do
  x <- getStateFull @s e
  x' <- f x
  setState @s x'

inputs ::
  forall t.
  Transaction t =>
  TransactionAuthor ->
  t ->
  TM.Map (InputsMap t)
inputs author t = TM.submap $ cells author t

outputs ::
  forall t.
  Transaction t =>
  TransactionAuthor ->
  t ->
  TM.Map (OutputsMap t)
outputs author t = TM.submap $ cells author t

type TotalSize = 32

type (::->) (k :: Symbol) (v :: Type) = k ':-> ComplexK v

data ComplexK v

class SizedType (ComplexDataKey s) => ComplexData s where
  type ComplexDataKey s :: Type
  type ComplexDataValue s :: Type
  filterComplex :: ComplexDataKey s -> ByteString -> Maybe (Either Text (ComplexDataValue s))

type ExtraKeySize s = TypeSize (ComplexDataKey s)

encodeKey ::
  forall t effs.
  Members '[Eff.Reader Namespace] effs =>
  Key t ->
  Eff effs TL.Text
encodeKey k = do
  n <- Eff.ask
  return $ encodeKey' n k

encodeKey' ::
  forall t n.
  KnownNat n =>
  Namespace ->
  Key' n t ->
  TL.Text
encodeKey' (Namespace n) k = n <> toHexTL (encodeKey'' k)

encodeKey'' ::
  forall t n.
  KnownNat n =>
  Key' n t ->
  ByteString
encodeKey'' (SKey k) = BS.take (fromInteger $ natVal (Proxy @n)) $ digestSimpleKey k
encodeKey'' (CKey k rest) = encodeKey'' k <> (BSS.unByteString . encodeSized) rest

digestSimpleKey :: forall k. SimpleKey k -> ByteString
digestSimpleKey (SimpleKey x) = BS.pack . BA.unpack $ digest
  where
    digest :: Digest SHA256 =
      hashlazy $
        serialise
          EncodedKey
            { keyName = T.pack $ symbolVal (Proxy @(TypeName k)),
              key = serialise x
            }

data EncodedKey = EncodedKey
  { keyName :: Text,
    key :: BSL.ByteString
  }
  deriving stock (Generic, Show)
  deriving anyclass (Serialise)
