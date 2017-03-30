-- language=c prefix='[C.block|' suffix='}|]'
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Network.Mosquitto where

import           Data.Coerce (coerce)
import           Data.Monoid ((<>))
-- import qualified Data.Vector.Storable as V
-- import qualified Data.Vector.Storable.Mutable as VM
import           Foreign.C.Types
import           Foreign.ForeignPtr (ForeignPtr, withForeignPtr, newForeignPtr_)
import           Foreign.Ptr ( Ptr, nullPtr )
import           Foreign.Storable (Storable)
import           Foreign.Marshal.Alloc ( alloca )

import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU

import           Language.C.Inline.TypeLevel

import           System.IO.Unsafe (unsafePerformIO)
import           Foreign.Storable
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Internal as BSI
import qualified Data.ByteString.Unsafe as BS

import           Network.Mosquitto.Internal.Types
import           Network.Mosquitto.Internal.Inline


C.context (C.baseCtx <> C.vecCtx <> C.funCtx <> mosquittoCtx)
C.include "<mosquitto.h>"


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

type instance C C8.ByteString = CChar

-- instance WithPtr C8.ByteString where
--   withPtr




newMosquitto :: Bool -> Maybe String -> Maybe a -> IO (Mosquitto a)
newMosquitto clearSession (fmap C8.pack -> userId) userData = do
   let fpUserId = case userId of
         Nothing -> \f -> f nullPtr
         Just bs -> BS.unsafeUseAsCString bs
   fpUserId $ \ptr -> do
       fp <- newForeignPtr_ <$> [C.exp|struct mosquitto *{
            mosquitto_new($(const char * ptr), $(bool clearSession), 0)
             }|]
       Mosquitto <$> fp


destroyMosquitto :: Mosquitto a -> IO ()
destroyMosquitto ms = withPtr ms $ \ptr ->
   [C.exp|void{
         mosquitto_destroy($(struct mosquitto *ptr))
     }|]
