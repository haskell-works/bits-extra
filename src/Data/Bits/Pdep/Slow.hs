module Data.Bits.Pdep.Slow
  ( SlowPdep(..)
  ) where

import Data.Bits
import GHC.Int
import GHC.Word

slowPdep64 :: Word64 -> Word64 -> Word64
slowPdep64 = slowPdep64' 0

lsb :: Word64 -> Word64
lsb src = fromIntegral ((fromIntegral (src `shiftL` 63) :: Int64) `shiftR` 63)

slowPdep64' :: Word64 -> Word64 -> Word64 -> Word64
slowPdep64' result src mask = if lowest /= 0
  then slowPdep64' newResult (src `shiftR` 1) (mask .&. complement lowest)
  else result
  where lowest    = (-mask) .&. mask
        newResult = (result .|. ((lsb src) .&. lowest))

-- | Bitwise parallel deposit (emulated).  Deposits bits from the source at the
-- locations described by the mask.
class SlowPdep a where
  slowPdep :: a -> a -> a

instance SlowPdep Word where
  slowPdep s m = fromIntegral (slowPdep64 (fromIntegral s) (fromIntegral m))

instance SlowPdep Word8 where
  slowPdep s m = fromIntegral (slowPdep64 (fromIntegral s) (fromIntegral m))

instance SlowPdep Word16 where
  slowPdep s m = fromIntegral (slowPdep64 (fromIntegral s) (fromIntegral m))

instance SlowPdep Word32 where
  slowPdep s m = fromIntegral (slowPdep64 (fromIntegral s) (fromIntegral m))

instance SlowPdep Word64 where
  slowPdep s m = fromIntegral (slowPdep64 (fromIntegral s) (fromIntegral m))
