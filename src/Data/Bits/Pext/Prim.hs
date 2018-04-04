{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}

module Data.Bits.Pext.Prim
  ( primPext
  , primPext8
  , primPext16
  , primPext32
  , primPext64
  , fastPextEnabled
  ) where

import GHC.Word

#if MIN_VERSION_base(4,11,0) && defined(BMI2_ENABLED)
import GHC.Prim
#else
import Data.Bits.Pext.Slow
#endif

primPext :: Word -> Word -> Word
primPext src mask = fromIntegral (primPext64 (fromIntegral src) (fromIntegral mask))
{-# INLINE primPext #-}

primPext64 :: Word64 -> Word64 -> Word64
#if MIN_VERSION_base(4,11,0) && defined(BMI2_ENABLED)
primPext64 (W64# src#) (W64# mask#) = W64# (pext64# src# mask#)
#else
primPext64 = slowPext
#endif
{-# INLINE primPext64 #-}

primPext32 :: Word32 -> Word32 -> Word32
#if MIN_VERSION_base(4,11,0) && defined(BMI2_ENABLED)
primPext32 (W32# src#) (W32# mask#) = W32# (pext32# src# mask#)
#else
primPext32 = slowPext
#endif
{-# INLINE primPext32 #-}

primPext16 :: Word16 -> Word16 -> Word16
primPext16 src mask = fromIntegral (primPext32 (fromIntegral src) (fromIntegral mask))
{-# INLINE primPext16 #-}

primPext8 :: Word8 -> Word8 -> Word8
primPext8 src mask = fromIntegral (primPext32 (fromIntegral src) (fromIntegral mask))
{-# INLINE primPext8 #-}

-- | Runtime flag indicating whether the 'pext' function is using the high-performance.
-- BMI2 instruction set.  A value of `False` indicates that `pext` is emulated.
fastPextEnabled :: Bool
#if MIN_VERSION_base(4,11,0) && defined(BMI2_ENABLED)
fastPextEnabled = True
#else
fastPextEnabled = False
#endif
{-# INLINE fastPextEnabled #-}
