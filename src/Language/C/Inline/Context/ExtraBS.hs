{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | A 'Context' is used to define the capabilities of the Template Haskell code
-- that handles the inline C code. See the documentation of the data type for
-- more details.
--
-- In practice, a 'Context' will have to be defined for each library that
-- defines new C types, to allow the TemplateHaskell code to interpret said
-- types correctly.

module Language.C.Inline.Context.ExtraBS
  ( -- * 'TypesTable'
    bsXCtx
  ) where

import           Language.C.Inline.Context
import           Control.Applicative ((<|>))
import           Control.Monad (mzero)
--import           Control.Monad.Trans.Class (lift)
--import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import           Data.Coerce
-- import           Data.Int (Int8, Int16, Int32, Int64)
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Data.Typeable (Typeable)
--import qualified Data.Vector.Storable as V
--import qualified Data.Vector.Storable.Mutable as VM
import           Data.Word (Word8, Word16, Word32, Word64)
import           Foreign.C.Types
import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Ptr (Ptr, FunPtr, freeHaskellFunPtr)
--import           Foreign.Storable (Storable)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
--import qualified Text.Parser.Token as Parser
-- import qualified Data.HashSet as HashSet

-- import           Language.C.Inline.FunPtr
import qualified Language.C.Types as C
import           Language.C.Inline.HaskellIdentifier

#if __GLASGOW_HASKELL__ < 710
import           Data.Monoid (Monoid(..))
import           Data.Traversable (traverse)
#endif

getHsVariable :: String -> HaskellIdentifier -> TH.ExpQ
getHsVariable err s = do
  mbHsName <- TH.lookupValueName $ unHaskellIdentifier s
  case mbHsName of
    Nothing -> fail $ "Cannot capture Haskell variable " ++ unHaskellIdentifier s ++
                      ", because it's not in scope. (" ++ err ++ ")"
    Just hsName -> TH.varE hsName

-- | 'bsCtx' serves exactly the same purpose as 'vecCtx', but only for
-- @bs-cstr@, it will always be null terminated @char*@.
bsXCtx :: Context
bsXCtx = mempty
#if __GLASGOW_HASKELL__ < 820
  { ctxAntiQuoters = Map.fromList
      [ ("bs-cstr", SomeAntiQuoter bsCStrAntiQuoter)
      ]
  }


bsCStrAntiQuoter :: AntiQuoter HaskellIdentifier
bsCStrAntiQuoter = AntiQuoter
  { aqParser = do
      hId <- C.parseIdentifier
      let cId = mangleHaskellIdentifier hId
      return (cId, C.Ptr [] (C.TypeSpecifier mempty (C.Char Nothing)), hId)
  , aqMarshaller = \_purity _cTypes cTy cId -> do
      case cTy of
        C.Ptr _ (C.TypeSpecifier _ (C.Char Nothing)) -> do
          hsTy <- [t| Ptr CChar |]
          hsExp <- getHsVariable "bsCtx" cId
          hsExp' <- [| \cont -> BS.useAsCString $(return hsExp) $ \ptr -> cont ptr  |]
          return (hsTy, hsExp')
        _ ->
          fail "impossible: got type different from `char *' (bsCtx)"
  }
#endif
