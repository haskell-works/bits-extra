{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}

module Data.Bits.Pext
  ( Pext(..)
  , fastPextEnabled
  ) where

import GHC.Word

import qualified Data.Bits.Pext.Prim as P

-- | Bitwise parallel extosit.  extosits bits from the source at the locations
-- described by the mask.
class Pext a where
  pext :: a -> a -> a

instance Pext Word where
  pext = P.primPext
  {-# INLINE pext #-}

instance Pext Word8 where
  pext = P.primPext8
  {-# INLINE pext #-}

instance Pext Word16 where
  pext = P.primPext16
  {-# INLINE pext #-}

instance Pext Word32 where
  pext = P.primPext32
  {-# INLINE pext #-}

instance Pext Word64 where
  pext = P.primPext64
  {-# INLINE pext #-}

-- | Runtime flag indicating whether the 'pext' function is using the high-performance.
-- BMI2 instruction set.  A value of `False` indicates that `pext` is emulated.
fastPextEnabled :: Bool
fastPextEnabled = P.fastPextEnabled
{-# INLINE fastPextEnabled #-}
