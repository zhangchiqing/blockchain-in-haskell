module Haskoin.Mining where

import Haskoin.Types
import Haskoin.Serialization
import Data.Time.Clock (NominalDiffTime)
import Data.Time.Clock.POSIX
import Crypto.Hash (hashlazy)
import Crypto.Number.Serialize(os2ip)
import qualified Data.Map as M

type TransactionPool = IO [Transaction]

-- Number of unit coin as mining reword
blockReward :: Integer
blockReward = 1000

-- Limit on the total number of transactions in each block
globalTransactionLimit :: Int
globalTransactionLimit = 1000

numBlocksToCalculateDifficulty :: Int
numBlocksToCalculateDifficulty = 100

-- a 46 bit number
genesisBlockDifficulty :: (Fractional a) => a
genesisBlockDifficulty = 0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF

-- Target to produce each block in about 10 seconds
targetTime :: (Fractional a) => a
targetTime = 10

-- Given a list of pending transactions, take
mineOn :: TransactionPool -> Account -> Blockchain -> IO Blockchain
mineOn pendingTransactions minerAccount parent = do
  allpending <- pendingTransactions
  let valids = validTransactions parent allpending
  let ts = take globalTransactionLimit valids
  loop ts 0
  where
    -- hash(B) <= M / D, where
    --   M = 2 ^ 160 - 1
    --   D's range = [1, M]
    validChain :: Blockchain -> Bool
    --validChain bc = difficulty bc < desiredDifficulty parent
    validChain bc = difficulty bc <= desiredDifficulty parent

    loop :: [Transaction] -> Integer -> IO Blockchain
    loop ts nonce = do
      now <- getPOSIXTime
      let header = BlockHeader {
            _miner = minerAccount,
            _parentHash = hash parent,
            _nonce = nonce,
            _minedAt = now
            }
          block = Block ts
          candidate = Node header block parent
      if validChain candidate
        then return candidate
        else loop ts (nonce + 1)

-- A 160 bit integer, ranges from [0, 2 ^ 160]
difficulty :: Blockchain -> Integer
difficulty = os2ip . hash

blockTimeAverage :: Blockchain -> NominalDiffTime
blockTimeAverage bc = average $ zipWith (-) times (tail times)
  where
    times = take numBlocksToCalculateDifficulty $ map _minedAt $ headers bc

average :: (Foldable f, Num a, Fractional a, Eq a) => f a -> a
average xs = sum xs `safeDiv` fromIntegral (length xs)

safeDiv :: (Num a, Eq a, Fractional a) => a -> a -> a
safeDiv n d = n / (if d == 0 then 1 else d)

-- BEWARE: O(n * k), where k = numBlocksToCalculateDifficulty
-- M / D
desiredDifficulty :: Blockchain -> Integer
desiredDifficulty bc = round $ loop bc
  where
    loop (Genesis _) = genesisBlockDifficulty
    loop x@(Node _ _ xs) = oldDifficulty / adjustmentFactor
      where
        oldDifficulty = loop xs
        adjustmentFactor = min 4.0 $ targetTime `safeDiv` blockTimeAverage x

makeGenesis :: IO Blockchain
makeGenesis = return $ Genesis $ Block []

headers :: Blockchain -> [BlockHeader]
headers (Genesis _) = []
headers (Node h _ parent) = h : headers parent

chains :: Blockchain -> [Blockchain]
chains x@(Genesis _) = [x]
chains x@(Node _ _ next) = x : chains next

hash :: Blockchain -> HaskoinHash
hash = hashlazy . serialize

balances :: Blockchain -> M.Map Account Integer
balances bc =
  let txns = bToList bc
      debits = map (\Transaction{ _from = acc, _amount = amount} -> (acc, -amount)) txns
      credits = map (\Transaction{ _to = acc, _amount = amount} -> (acc, amount)) txns
      minings = map (\h -> (_miner h, blockReward)) $ headers bc
  in M.fromListWith (+) $ debits ++ credits ++ minings

-- Reject wrong accounts and insufficient funds
validTransactions :: Blockchain -> [Transaction] -> [Transaction]
validTransactions bc txns =
  let accounts = balances bc
      validTxn txn = case M.lookup (_from txn) accounts of
        Nothing -> False
        Just balance -> balance >= _amount txn
  in filter validTxn txns
