-- sudo docker run -d -p 8022:22 --name test_sshd rastasheep/ubuntu-sshd:14.04
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Network.Socket 
import Data.List 
import qualified Data.ByteString.Char8 as B8
import Control.Lens
import Control.Monad
import System.Environment


import Network.SSH.Client.SimpleSSH

main :: IO ()
main =  let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
        in do
                (h1:(p1:_)) <- getArgs
                addr:_ <- getAddrInfo (Just hints) (Just h1) (Just p1)
                sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
                Network.Socket.connect sock (addrAddress addr)
                name <- getSocketName sock
                print name
                pierName <- getPeerName sock >>= (pure . ( _2 %~ (drop 1) ) . (span ( /= ':')) . show)
                print pierName
                void $ send2 sock "echo test"
                


send2 :: Socket -> B8.ByteString -> IO Int
send2 sock msg = do
    (h, p) <- getPeerName sock >>= (pure . ( _2 %~ (drop 1) ) . (span ( /= ':')) . show)
    (h1:(p1:(k1:(l1:(ps1:(c1:_)))))) <- getArgs
    rs <- runSimpleSSH $ 
            withSessionPassword 
                h
                (read p)
                k1
                l1 
                p1
                (flip execCommand c1)
    print rs
    pure 0

recv2 :: Socket -> Int -> IO B8.ByteString
recv2 _ _ = pure ""
