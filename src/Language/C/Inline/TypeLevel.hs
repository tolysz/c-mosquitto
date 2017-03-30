{-# LANGUAGE TypeFamilies #-}

module Language.C.Inline.TypeLevel where

import Foreign.C.Types
import Foreign.Ptr ( Ptr, nullPtr )
import Data.Int ( Int32 )
import GHC.TypeLits

import Control.Monad.Primitive ( PrimMonad, PrimState )

-- | Wrapper for mutable values
newtype Mut a s = Mut { unMut :: a }

type family Mutable (a :: *) :: * -> *

class FreezeThaw a where
    freeze :: (PrimMonad m) => Mutable a (PrimState m) -> m a
    thaw   :: (PrimMonad m) => a -> m (Mutable a (PrimState m))

    unsafeFreeze :: (PrimMonad m) => Mutable a (PrimState m) -> m a
    unsafeThaw   :: (PrimMonad m) => a -> m (Mutable a (PrimState m))

type family C (a :: *) :: *

type instance C (Maybe a) = C a
type instance C (Mut a s) = C a

-- | Perform an IO action with a pointer to the C equivalent of a value
class WithPtr a where
    -- | Perform an action with a temporary pointer to the underlying
    -- representation of @a@
    --
    -- The pointer is not guaranteed to be usuable outside the scope of this
    -- function. The same warnings apply as for 'withForeignPtr'.
    withPtr :: a -> (Ptr (C a) -> IO b) -> IO b

-- | 'Nothing' is represented as a 'nullPtr'.
instance (WithPtr a) => WithPtr (Maybe a) where
    withPtr Nothing    f = f nullPtr
    withPtr (Just obj) f = withPtr obj f

-- | Mutable types use the same underlying representation as unmutable types.
instance (WithPtr a) => WithPtr (Mut a s) where
    withPtr = withPtr . unMut

-- | Information about the storage requirements of values in C
--
-- This class assumes that the type @a@ is merely a symbol that corresponds with
-- a type in C.
class CSizeOf a where
    -- | Computes the storage requirements (in bytes) of values of
    -- type @a@ in C.
    cSizeOf :: proxy a -> Int

--------------------------------------------------------------------------------

-- | Types of which a value can be constructed from a pointer to the C
-- equivalent of that value
--
-- Used to wrap values created in C.
class FromPtr a where
    fromPtr :: IO (Ptr (C a)) -> IO a
