{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}

module Data.Bits.Pdep.Prim
  ( primPdep
  , primPdep8
  , primPdep16
  , primPdep32
  , primPdep64
  , fastPdepEnabled
  ) where

import GHC.Word

#if MIN_VERSION_base(4,11,0) && defined(BMI2_ENABLED)
import GHC.Prim
#else
import Data.Bits.Pdep.Slow
#endif

primPdep :: Word -> Word -> Word
primPdep src mask = fromIntegral (primPdep64 (fromIntegral src) (fromIntegral mask))
{-# INLINE primPdep #-}

primPdep64 :: Word64 -> Word64 -> Word64
#if MIN_VERSION_base(4,11,0) && defined(BMI2_ENABLED)
primPdep64 (W64# src#) (W64# mask#) = W64# (pdep64# src# mask#)
#else
primPdep64 = slowPdep
#endif
{-# INLINE primPdep64 #-}

primPdep32 :: Word32 -> Word32 -> Word32
#if MIN_VERSION_base(4,11,0) && defined(BMI2_ENABLED)
primPdep32 (W32# src#) (W32# mask#) = W32# (pdep32# src# mask#)
#else
primPdep32 = slowPdep
#endif
{-# INLINE primPdep32 #-}

primPdep16 :: Word16 -> Word16 -> Word16
primPdep16 src mask = fromIntegral (primPdep32 (fromIntegral src) (fromIntegral mask))
{-# INLINE primPdep16 #-}

primPdep8 :: Word8 -> Word8 -> Word8
primPdep8 src mask = fromIntegral (primPdep32 (fromIntegral src) (fromIntegral mask))
{-# INLINE primPdep8 #-}

-- | Runtime flag indicating whether the 'pdep' function is using the high-performance.
-- BMI2 instruction set.  A value of `False` indicates that `pdep` is emulated.
fastPdepEnabled :: Bool
#if MIN_VERSION_base(4,11,0) && defined(BMI2_ENABLED)
fastPdepEnabled = True
#else
fastPdepEnabled = False
#endif
{-# INLINE fastPdepEnabled #-}
