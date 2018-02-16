{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Mosquitto.Internal.Types where

import           Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import           Foreign.Ptr (Ptr, FunPtr)
import           Foreign.C.Types
import           Language.C.Inline.TypeLevel
import qualified Language.C.Types as C
import qualified Language.C.Inline.Context as C
import           Data.ByteString (ByteString)

import qualified Data.Map as M
import           Foreign.Storable

-- haskell types
newtype Mosquitto a = Mosquitto { unMosquitto :: ForeignPtr (C (Mosquitto a))}
-- newtype Message = Message { unMessage :: Ptr (C Message)}
newtype RawMessage = RawMessage { unRawMessage :: Ptr (C RawMessage)}

data Message = Message
  { mid     :: Int
  , topic   :: String
  , payload :: ByteString
  , qos     :: Int
  , retain  :: Bool
  } deriving (Eq, Show, Read)


data C'Mosquitto
data C'Message

-- glue
type instance C (Mosquitto a) = C'Mosquitto
type instance C RawMessage    = C'Message

instance WithPtr (Mosquitto a) where
    withPtr = withForeignPtr . unMosquitto
instance WithPtr RawMessage where
    withPtr m f = f (unRawMessage m)


mosquittoTypesTable :: C.TypesTable
mosquittoTypesTable = M.fromList
   [ ( C.Struct   "mosquitto"         , [t| C'Mosquitto |] )
   , ( C.Struct   "mosquitto_message" , [t| C'Message |] )
   , ( C.TypeName "bool"              , [t| Bool |] )
   ]

type OnMessage = Message -> IO ()
type COnMessage = Ptr C'Mosquitto -> Ptr () -> Ptr C'Message -> IO ()
foreign import ccall "wrapper"
  mkCOnMessage :: COnMessage -> IO (FunPtr COnMessage)

type OnLog = Int -> String -> IO ()
type COnLog = Ptr C'Mosquitto ->  Ptr () -> CInt -> Ptr CChar -> IO ()
foreign import ccall "wrapper"
  mkCOnLog :: COnLog -> IO (FunPtr COnLog)

type OnConnection = Int -> IO ()
type COnConnection = Ptr C'Mosquitto ->  Ptr () -> CInt -> IO ()
foreign import ccall "wrapper"
  mkCOnConnection :: COnConnection -> IO (FunPtr COnConnection)

type OnSubscribe = Int -> [Int] -> IO ()
type COnSubscribe = Ptr C'Mosquitto ->  Ptr () -> CInt -> CInt -> Ptr CInt -> IO ()
foreign import ccall "wrapper"
  mkCOnSubscribe :: COnSubscribe -> IO (FunPtr COnSubscribe)

type OnPublish = Int -> IO ()
type COnPublish = Ptr C'Mosquitto -> Ptr () -> CInt -> IO ()
foreign import ccall "wrapper"
  mkCOnPublish :: COnPublish -> IO (FunPtr COnPublish)
