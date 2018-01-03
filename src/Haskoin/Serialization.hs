{-# LANGUAGE StandaloneDeriving, TypeSynonymInstances, FlexibleInstances, UndecidableInstances, DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Haskoin.Serialization where

import Haskoin.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Time.Clock.POSIX
import Data.Binary
import GHC.Generics
import Crypto.Hash
import Data.ByteArray (convert)


deriving instance Generic Account
instance Binary Account

deriving instance Generic Transaction
instance Binary Transaction

deriving instance Generic (BlockF a)
instance (Binary a) => Binary (BlockF a)

instance Binary POSIXTime where
  get = fromInteger <$> (get :: Get Integer)
  put x = put (round x :: Integer)

deriving instance Generic BlockHeader
instance Binary BlockHeader

deriving instance Generic (MerkleTree a)
instance (Binary a) => Binary (MerkleTree a)

instance Binary HaskoinHash where
  get = do
    mDigest <- digestFromByteString <$> (get :: Get BS.ByteString)
    case mDigest of
      Nothing -> fail "Not a valid digest"
      Just digest -> return digest
  put digest = put (convert digest :: BS.ByteString)

deserialize :: BSL.ByteString -> Blockchain
deserialize = decode

serialize :: Blockchain -> BSL.ByteString
serialize = encode
