{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}

module Data.Bits.Pext
  ( Pext(..)
  , fastPextEnabled
  ) where

import GHC.Prim
import GHC.Word

-- | Bitwise parallel extract.  Extract bits from the source at the locations
-- described by the mask.
class Pext a where
  pext :: a -> a -> a

instance Pext Word where
  pext (W#   src#) (W#   mask#) = W#   (pext#   src# mask#)
  {-# INLINE pext #-}

instance Pext Word8 where
  pext src mask = fromIntegral (pext (fromIntegral src) (fromIntegral mask) :: Word32)
  {-# INLINE pext #-}

instance Pext Word16 where
  pext src mask = fromIntegral (pext (fromIntegral src) (fromIntegral mask) :: Word32)
  {-# INLINE pext #-}

#if MIN_VERSION_base(4,11,0)
instance Pext Word32 where
  pext (W32# src#) (W32# mask#) = W32# (pext32# src# mask#)
  {-# INLINE pext #-}

instance Pext Word64 where
  pext (W64# src#) (W64# mask#) = W64# (pext64# src# mask#)
  {-# INLINE pext #-}
#else
instance Pext Word32 where
  pext = slowPext
  {-# INLINE pext #-}

instance Pext Word64 where
  pext = slowPext
  {-# INLINE pext #-}
#endif

fastPextEnabled :: Bool
#if MIN_VERSION_base(4,11,0) && defined(BMI2_ENABLED)
fastPextEnabled = True
#else
fastPextEnabled = False
#endif
