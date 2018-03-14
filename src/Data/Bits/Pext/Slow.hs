module Data.Bits.Pext.Slow
  ( SlowPext(..)
  ) where

import Data.Bits
import GHC.Word

slowPext64 :: Word64 -> Word64 -> Word64
slowPext64 = slowPext64' 0 0 0

slowPext64' :: Word64 -> Int -> Int -> Word64 -> Word64 -> Word64
slowPext64' result offset index src mask = if index /= 64
  then if maskBit /= 0
          then slowPext64' nextResult (offset + 1) (index + 1) src mask
          else slowPext64' result      offset      (index + 1) src mask
  else result
  where srcBit      = (src  `shiftR` index) .&. 1
        maskBit     = (mask `shiftR` index) .&. 1
        nextResult  = result .|. (srcBit `shiftL` offset)

-- | Bitwise parallel extract (emulated).  Extract bits from the source at
-- the locations described by the mask.
class SlowPext a where
  slowPext :: a -> a -> a

instance SlowPext Word where
  slowPext s m = fromIntegral (slowPext64 (fromIntegral s) (fromIntegral m))

instance SlowPext Word8 where
  slowPext s m = fromIntegral (slowPext64 (fromIntegral s) (fromIntegral m))

instance SlowPext Word16 where
  slowPext s m = fromIntegral (slowPext64 (fromIntegral s) (fromIntegral m))

instance SlowPext Word32 where
  slowPext s m = fromIntegral (slowPext64 (fromIntegral s) (fromIntegral m))

instance SlowPext Word64 where
  slowPext s m = fromIntegral (slowPext64 (fromIntegral s) (fromIntegral m))
