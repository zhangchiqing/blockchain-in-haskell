{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Haskoin.Cli.Mine where

import Prelude
import System.Environment (getArgs)
import System.Directory (copyFile, doesFileExist)

import Haskoin.Types (Blockchain, Account(..))
import Haskoin.Mining (mineOn, makeGenesis)
import Data.Binary (encodeFile, decodeFile, Binary)
import Control.Monad (forever)

defaultChainFile :: FilePath
defaultChainFile = "main.chain"

defaultAccount :: String
defaultAccount = "10"

main :: IO ()
main = do
  args <- getArgs
  let (fileName, accountS) = case args of
        [] -> (defaultChainFile, defaultAccount)
        [filename] -> (filename, defaultAccount)
        [filename, account] -> (filename, account)
        _ -> error "Usage: mine [filename] [account]"
      swapFile = fileName ++ ".tmp"
      txnPool = return []
      acc = Account $ read accountS
  forever $ do
    chain <- loadOrCreate fileName makeGenesis :: IO Blockchain
    newChain <- mineOn txnPool acc chain
    encodeFile swapFile newChain
    copyFile swapFile fileName
    print "Block mined and saved!"

loadOrCreate :: Binary a => FilePath -> IO a -> IO a
loadOrCreate filename init = do
  exists <- doesFileExist filename
  if exists
    then decodeFile filename
    else do
      x <- init
      encodeFile filename x
      return x
