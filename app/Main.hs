module Main where

import qualified Network.Mosquitto as M

main :: IO ()
main = M.withMosquittoLibrary $ do
  print "Some Action"
  print M.version
  m <- M.newMosquitto False Nothing Nothing

  M.destroyMosquitto m
  print "The end"
