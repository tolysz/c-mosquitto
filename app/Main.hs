{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where

import           Control.Concurrent
import           Control.Monad
import qualified Network.Mosquitto as M
import           Network.Mosquitto.Internal.Types

import           Control.Applicative
import           Options

data MainOptions = MainOptions
    { caCert    :: String
    , userCert  :: String
    , userKey   :: String
    , server    :: String
    , serverName :: String
    , port      :: Int
    , keepAlive :: Int
    }

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "ca" "rootCA.pem"
            "server's CA"
        <*> simpleOption "cert" "cert.pem"
            "client cert"
        <*> simpleOption "key" "cert.key"
            "client key"
        <*> simpleOption "server" "localhost"
            "server"
        <*> simpleOption "name" "server-demo"
            "server's name"
        <*> simpleOption "port" 8883
            "server's port"
        <*> simpleOption "keep-alive" 1200
            "server's port"

main :: IO ()
main = runCommand $ \MainOptions{..} args -> M.withMosquittoLibrary $ do
  print M.version

  m <- M.newMosquitto True serverName (Just ())
  M.setTls m caCert userCert userKey
  M.setTlsInsecure m True

  M.onMessage m print
  M.onLog m $ const putStrLn
  M.onConnect m $ \c -> do
           print c
           M.subscribe m 0 "#"

  M.onDisconnect m print
  M.onSubscribe m $ curry print

  M.connect m server port keepAlive

  forkIO $ forever $ do
    M.publish m False 0 "hello" "bla"
    threadDelay 5000000

  M.loopForever m
  M.destroyMosquitto m
  print "The end"
