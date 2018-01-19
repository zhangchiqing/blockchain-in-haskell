import Haskoin.Types
import Haskoin.Mining
import qualified Data.ByteString.Base16.Lazy as BSBL
import qualified Data.ByteString.Lazy as BSL
import Data.Binary (encode, decode)

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Types.Account" $ do
    it "Account should have Eq instance" $ do
      testAccount `shouldBe` testAccount

    it "Account should have Ord instance" $ do
      Account 10 > Account 9

  describe "Types.Transaction" $ do
    it "Transaction should have Eq instance" $ do
      testTransaction == testTransaction

  -- describe "Types.BlockF" $ do
  --   it "BlockF should have Monoid instance" $ do
  --     (Block [1]) ++ (Block [2, 3]) `shouldBe` Block [1, 2, 3]


testAccount :: Account
testAccount = Account 10

testTransaction :: Transaction
testTransaction = Transaction {
    _from = Account 10
  , _to = Account 20
  , _amount = 100
  }

testMining :: IO Blockchain
testMining = do
  let txnPool = return []
  chain <- makeGenesis
  chain <- mineOn txnPool 0 chain
  chain <- mineOn txnPool 0 chain
  chain <- mineOn txnPool 0 chain
  chain <- mineOn txnPool 0 chain
  chain <- mineOn txnPool 0 chain
  return chain

decodedChain :: Blockchain -> Blockchain
decodedChain = decode . encode

canDecode :: IO Bool
canDecode = do
  chain <- testMining
  let newChain = (decode $ encode chain) :: Blockchain
  return $ chain == newChain

hexSerialize :: IO BSL.ByteString
hexSerialize = do
  chain <- testMining
  return $ BSBL.encode $ encode chain
