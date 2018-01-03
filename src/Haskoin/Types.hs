{-# LANGUAGE GeneralizedNewtypeDeriving, NoImplicitPrelude, DeriveTraversable, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances, FlexibleInstances #-}

module Haskoin.Types where

import Protolude
import Crypto.Hash
import Data.Time.Clock.POSIX

newtype Account = Account Integer deriving (Eq, Show, Num, Ord)

data Transaction = Transaction {
  _from   :: Account,
  _to     :: Account,
  _amount :: Integer
  } deriving (Eq, Show)

newtype BlockF a = Block [a] deriving (Eq, Show, Foldable, Functor, Monoid)
-- newtype BlockF a = Block [a] deriving (Eq, Show, Foldable, Traversable, Functor, Monoid)
-- Block [Transaction]
type BlockBody = BlockF Transaction

type HaskoinHash = Digest SHA1

data BlockHeader = BlockHeader {
  -- _miner       :: Account
  _miner       :: Account,
  _parentHash  :: HaskoinHash,
  _nonce       :: Integer,
  _minedAt     :: POSIXTime
  } deriving (Eq, Show)


data MerkleTree a = Genesis a
                 | Node BlockHeader a (MerkleTree a)
                 deriving (Eq, Show, Foldable, Functor)
              -- deriving (Eq, Show, Foldable, Traversable, Functor)

type Blockchain = MerkleTree BlockBody

-- bToList :: Blockchain -> Block
-- bToList Genesis = mempty
-- bToList (Node _ block l) = mappend block $ bToList l

bToList :: Blockchain -> [Transaction]
bToList (Genesis (Block txs)) = txs
bToList (Node _ (Block txs) l) = mappend txs $ bToList l
