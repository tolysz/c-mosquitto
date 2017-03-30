{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}


module Network.Mosquitto.Internal.Types where

import           Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import           Foreign.Ptr (Ptr)
import           Language.C.Inline.TypeLevel
import qualified Language.C.Types as C
import qualified Language.C.Inline.Context as C

import qualified Data.Map as M

-- haskell types
newtype Mosquitto a = Mosquitto {unMosquitto :: ForeignPtr (C (Mosquitto a))}

-- C proxy type
data C'Mosquitto

-- glue
type instance C (Mosquitto a) = C'Mosquitto

instance WithPtr (Mosquitto a) where
    withPtr = withForeignPtr . unMosquitto

mosquittoTypesTable :: C.TypesTable
mosquittoTypesTable = M.fromList
   [ ( C.Struct   "mosquitto"    , [t| C'Mosquitto |] )
   , ( C.TypeName "bool"         , [t| Bool |] )
   ]
