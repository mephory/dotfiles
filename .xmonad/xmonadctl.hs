{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import XMonad
import Data.ByteString.Lazy.UTF8 (toString)
import Network.Simple.TCP
import qualified Data.ByteString.Char8 as B

main = sendCommand "XMONAD_COMMAND" =<< getArgs

sendCommand addr command = do
  port <- getRandomPort
  sendCommand' addr $ toString $ encode ([port] ++ command)

getRandomPort = return "2099"

sendCommand' addr command = do
  d <- openDisplay ""
  rw <- rootWindow d $ defaultScreen d
  a <- internAtom d addr False
  m <- internAtom d command False
  listen (Host "127.0.0.1") "2099" $ \(sock, remoteAddr) -> do
    test <- accept sock \(s, ra) -> do
      resp <- recv s 4096
      return $ maybe "" id resp
    B.putStrLn test
    return ()
  allocaXEvent \e -> do
    setEventType e clientMessage
    setClientMessageEvent e rw a 32 m 0
    sendEvent d rw False structureNotifyMask e
    sync d False
