-- sudo docker run -d -p 8022:22 --name test_sshd rastasheep/ubuntu-sshd:14.04
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Network.Socket 
import Data.List 
import qualified Data.ByteString.Char8 as B8
import Control.Lens
import Control.Monad


import Network.SSH.Client.SimpleSSH

main :: IO ()
main =  let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
        in do
                addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "8022")
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
    rs <- runSimpleSSH $ 
            withSessionPassword 
                "localhost" 
                8022
                "/home/petr/" 
                "root" 
                "root" 
                (flip execCommand (B8.unpack msg))
    print rs
    pure 0

recv2 :: Socket -> Int -> IO B8.ByteString
recv2 _ _ = pure ""
