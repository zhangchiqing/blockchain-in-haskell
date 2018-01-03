import Haskoin.Types
import Haskoin.Mining
import qualified Data.ByteString.Base16.Lazy as BSBL
import qualified Data.ByteString.Lazy as BSL
import Data.Binary (encode, decode)


main :: IO ()
main = putStrLn "Test suite not yet implemented"

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
