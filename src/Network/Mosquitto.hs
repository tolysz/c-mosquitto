{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Mosquitto where

import           Data.Coerce (coerce)
import           Data.Monoid ((<>))
import           Control.Monad
import           Foreign.C.Types
import           Foreign.ForeignPtr (ForeignPtr, withForeignPtr, newForeignPtr_)
import           Foreign.Ptr ( Ptr, nullPtr, castPtr, FunPtr)
import           Foreign.Marshal.Alloc ( alloca )
import           Foreign.C.String (peekCString, peekCStringLen)

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU

import           Language.C.Inline.TypeLevel

import           System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BS

import           Network.Mosquitto.Internal.Types
import           Network.Mosquitto.Internal.Inline
import           Foreign.Storable

C.context (C.baseCtx <> C.vecCtx <> C.funCtx <> mosquittoCtx)
C.include "<stdio.h>"
C.include "<mosquitto.h>"

c'MessageToMMessage :: Ptr C'Message -> IO Message
c'MessageToMMessage ptr =
   Message
    <$> (fromIntegral <$> [C.exp| int { $(struct mosquitto_message * ptr)->mid}  |])
    <*> (peekCString =<< [C.exp| char * {$(struct mosquitto_message * ptr) -> topic } |])
    <*> (C8.packCStringLen =<< (,)
             <$> [C.exp| char * {$(struct mosquitto_message * ptr) -> payload } |]
             <*> fmap fromIntegral [C.exp| int {$(struct mosquitto_message * ptr) -> payloadlen } |])
    <*> (fromIntegral <$> [C.exp| int { $(struct mosquitto_message * ptr)->qos}  |])
    <*> [C.exp| bool { $(struct mosquitto_message * ptr)->retain}  |]

{-# NOINLINE init #-}
init = [C.exp| int{ mosquitto_lib_init() }|]

{-# NOINLINE cleanup #-}
cleanup = [C.exp| int{ mosquitto_lib_cleanup() }|]

withMosquittoLibrary :: IO a -> IO a
withMosquittoLibrary f = Network.Mosquitto.init *> f <* cleanup

{-# NOINLINE version #-}
version :: (Int, Int, Int)
version = unsafePerformIO $
  alloca $ \a -> alloca $ \b -> alloca $ \c -> do
      [C.block|void{ mosquitto_lib_version($(int *a),$(int *b),$(int *c)); }|]
      (,,) <$> peek' a
           <*> peek' b
           <*> peek' c
 where
   peek' x = fromIntegral <$> peek x

newMosquitto :: Bool -> String -> Maybe a -> IO (Mosquitto a)
newMosquitto clearSession (C8.pack -> userId) _userData = do
   fp <- newForeignPtr_ <$> [C.block|struct mosquitto *{
        struct mosquitto * p =
          mosquitto_new( $bs-ptr:userId
                       , $(bool clearSession)
                       , 0 // (void * ptrUserData)
                       );
        mosquitto_threaded_set(p, true);
        return p;
      }|]

   Mosquitto <$> fp

destroyMosquitto :: Mosquitto a -> IO ()
destroyMosquitto ms = withPtr ms $ \ptr ->
   [C.exp|void{
         mosquitto_destroy($(struct mosquitto *ptr))
     }|]

setTls :: Mosquitto a -> String -> String -> String -> IO ()
setTls mosq (C8.pack -> caFile) (C8.pack -> certFile) (C8.pack -> keyFile) =
  withPtr mosq $ \pMosq ->
       [C.exp|void{
               mosquitto_tls_set( $(struct mosquitto *pMosq)
                                , $bs-ptr:caFile
                                , 0
                                , $bs-ptr:certFile
                                , $bs-ptr:keyFile
                                , 0
                                )
       }|]

setReconnectDelay 
  :: Mosquitto a -- ^ mosquitto instance
  -> Bool        -- ^ exponential backoff
  -> Int         -- ^ initial backoff 
  -> Int         -- ^ maximum backoff
  -> IO Int
setReconnectDelay mosq  exponential (fromIntegral -> reconnectDelay) (fromIntegral -> reconnectDelayMax) =
  fmap fromIntegral <$> withPtr mosq $ \pMosq ->
       [C.exp|int{
             mosquitto_reconnect_delay_set
               ( $(struct mosquitto *pMosq)
               , $(int reconnectDelay)
               , $(int reconnectDelayMax)
               , $(bool exponential)
               )
        }|]

connect :: Mosquitto a -> String -> Int -> Int -> IO Int
connect mosq (C8.pack -> hostname) (fromIntegral -> port) (fromIntegral -> keepAlive) =
  fmap fromIntegral <$> withPtr mosq $ \pMosq ->
       [C.exp|int{
               mosquitto_connect( $(struct mosquitto *pMosq)
                                , $bs-ptr:hostname
                                , $(int port)
                                , $(int keepAlive)
                                )
             }|]

onSubscribe :: Mosquitto a -> OnSubscribe -> IO ()
onSubscribe mosq onSubscribe =  do
  on_subscribe <- mkCOnSubscribe $ \_ _ mid (fromIntegral -> ii) iis ->
      onSubscribe (fromIntegral mid) =<< mapM (fmap fromIntegral . peekElemOff iis) [0..ii-1]
  withPtr mosq $ \pMosq ->
     [C.block|void{
        mosquitto_subscribe_callback_set
            ( $(struct mosquitto *pMosq)
            , $(void (*on_subscribe)(struct mosquitto *,void *, int, int, const int *))
            );
       }|]


onConnect :: Mosquitto a -> OnConnection -> IO ()
onConnect mosq onConnect =  do
  on_connect <- mkCOnConnection $ \_ _ ii -> onConnect (fromIntegral ii)
  withPtr mosq $ \pMosq ->
     [C.block|void{
        mosquitto_connect_callback_set
            ( $(struct mosquitto *pMosq)
            , $(void (*on_connect)(struct mosquitto *,void *, int))
            );
       }|]

onDisconnect :: Mosquitto a -> OnConnection -> IO ()
onDisconnect mosq onDisconnect =  do
  on_disconnect <- mkCOnConnection $ \_ _ ii -> onDisconnect (fromIntegral ii)
  withPtr mosq $ \pMosq ->
     [C.block|void{
        mosquitto_disconnect_callback_set
            ( $(struct mosquitto *pMosq)
            , $(void (*on_disconnect)(struct mosquitto *,void *, int))
            );
       }|]

onLog :: Mosquitto a -> OnLog -> IO ()
onLog mosq onLog =  do
  on_log <- mkCOnLog $ \_ _ ii mm -> onLog (fromIntegral ii) =<< peekCString mm
  withPtr mosq $ \pMosq ->
     [C.block|void{
        mosquitto_log_callback_set
            ( $(struct mosquitto *pMosq)
            , $(void (*on_log)(struct mosquitto *,void *, int, const char *))
            );
       }|]

onMessage :: Mosquitto a -> OnMessage -> IO ()
onMessage mosq onMessage =  do
    on_message <- mkCOnMessage $ \_ _ mm -> (onMessage =<< c'MessageToMMessage mm)
    withPtr mosq $ \pMosq ->
     [C.block|void{
        mosquitto_message_callback_set
            ( $(struct mosquitto *pMosq)
            , $(void (*on_message)(struct mosquitto *, void *, const struct mosquitto_message *))
            );
       }|]

loop :: Mosquitto a -> IO ()
loop mosq =
  withPtr mosq $ \pMosq ->
    [C.exp|void{
             mosquitto_loop($(struct mosquitto *pMosq), -1, 1)
        }|]

loopForever :: Mosquitto a -> IO ()
loopForever mosq =
  withPtr mosq $ \pMosq ->
       [C.exp|void{
             mosquitto_loop_forever($(struct mosquitto *pMosq), -1, 1)
        }|]

setTlsInsecure :: Mosquitto a -> Bool -> IO ()
setTlsInsecure mosq isInsecure =
  withPtr mosq $ \pMosq ->
       [C.exp|void{
             mosquitto_tls_insecure_set($(struct mosquitto *pMosq), $(bool isInsecure))
        }|]

setWill :: Mosquitto a -> Bool -> Int -> String -> S.ByteString -> IO Int
setWill mosq retain (fromIntegral -> qos) (C8.pack -> topic) payload =
  fmap fromIntegral <$> withPtr mosq $ \pMosq ->
       [C.exp|int{
             mosquitto_will_set
               ( $(struct mosquitto *pMosq)
               , $bs-ptr:topic
               , $bs-len:payload
               , $bs-ptr:payload
               , $(int qos)
               , $(bool retain)
               )
        }|]

clearWill :: Mosquitto a -> IO Int
clearWill mosq = fmap fromIntegral <$> withPtr mosq $ \pMosq ->
       [C.exp|int{
             mosquitto_will_clear($(struct mosquitto *pMosq))
        }|]

publish :: Mosquitto a -> Bool -> Int -> String -> S.ByteString -> IO ()
publish mosq retain (fromIntegral -> qos) (C8.pack -> topic) payload =
  withPtr mosq $ \pMosq ->
       [C.exp|void{
             mosquitto_publish
               ( $(struct mosquitto *pMosq)
               , 0
               , $bs-ptr:topic
               , $bs-len:payload
               , $bs-ptr:payload
               , $(int qos)
               , $(bool retain)
               )
        }|]

subscribe :: Mosquitto a -> Int -> String -> IO ()
subscribe mosq (fromIntegral -> qos) (C8.pack -> topic) =
  withPtr mosq $ \pMosq ->
       [C.exp|void{
             mosquitto_subscribe
               ( $(struct mosquitto *pMosq)
               , 0
               , $bs-ptr:topic
               , $(int qos)
               )
        }|]

