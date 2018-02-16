# c-mosquitto

[Full code](https://github.com/tolysz/c-mosquitto/blob/master/app/Main.hs)
```haskell
main :: IO ()
main = runCommand $ \MainOptions{..} args -> M.withMosquittoLibrary $ do
  print M.version

  m <- M.newMosquitto True "server" (Just ())
  M.setTls m caCert userCert userKey
  M.setTlsInsecure m True

  -- callbacks
  M.onMessage m print
  M.onLog m $ const putStrLn
  M.onConnect m print
  M.onDisconnect m print
  M.onSubscribe m $ curry print

  M.connect m server port keepAlive

  M.subscribe m 0 "rcv/#"

  forkIO $ forever $ do
    M.publish m False 0 "hello" "bla"
    threadDelay 5000000

  M.loopForever m
  M.destroyMosquitto m
  print "The end"
```

# Hacking

Documentation for used C library can be found at <https://mosquitto.org/api/files/mosquitto-h.html>.
