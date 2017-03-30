
module Network.Mosquitto.Internal.Inline where

import qualified Language.C.Inline.Context as C
import Data.Monoid ( (<>) )

import Network.Mosquitto.Internal.Types

mosquittoCtx :: C.Context
mosquittoCtx = C.bsCtx <> C.vecCtx <> ctx
  where
    ctx = mempty { C.ctxTypesTable = mosquittoTypesTable }
