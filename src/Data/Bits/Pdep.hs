{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}

module Data.Bits.Pdep
  ( Pdep(..)
  , fastPdepEnabled
  ) where

import GHC.Prim
import GHC.Word

-- | Bitwise parallel deposit.  Deposits bits from the source at the locations
-- described by the mask.
class Pdep a where
  pdep :: a -> a -> a

instance Pdep Word where
  pdep (W#   src#) (W#   mask#) = W#   (pdep#   src# mask#)
  {-# INLINE pdep #-}

instance Pdep Word8 where
  pdep src mask = fromIntegral (pdep (fromIntegral src) (fromIntegral mask) :: Word32)
  {-# INLINE pdep #-}

instance Pdep Word16 where
  pdep src mask = fromIntegral (pdep (fromIntegral src) (fromIntegral mask) :: Word32)
  {-# INLINE pdep #-}

#if MIN_VERSION_base(4,11,0)
instance Pdep Word32 where
  pdep (W32# src#) (W32# mask#) = W32# (pdep32# src# mask#)
  {-# INLINE pdep #-}

instance Pdep Word64 where
  pdep (W64# src#) (W64# mask#) = W64# (pdep64# src# mask#)
  {-# INLINE pdep #-}
#else
instance Pdep Word32 where
  pdep = slowPdep
  {-# INLINE pdep #-}

instance Pdep Word64 where
  pdep = slowPdep
  {-# INLINE pdep #-}
#endif

fastPdepEnabled :: Bool
#if MIN_VERSION_base(4,11,0) && defined(BMI2_ENABLED)
fastPdepEnabled = True
#else
fastPdepEnabled = False
#endif
