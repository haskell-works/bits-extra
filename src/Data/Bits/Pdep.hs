{-# LANGUAGE CPP       #-}
{-# LANGUAGE MagicHash #-}

module Data.Bits.Pdep
  ( Pdep(..)
  , fastPdepEnabled
  ) where

import GHC.Word

import qualified Data.Bits.Pdep.Prim as P

-- | Bitwise parallel deposit.  Deposits bits from the source at the locations
-- described by the mask.
class Pdep a where
  pdep :: a -> a -> a

instance Pdep Word where
  pdep = P.primPdep
  {-# INLINE pdep #-}

instance Pdep Word8 where
  pdep = P.primPdep8
  {-# INLINE pdep #-}

instance Pdep Word16 where
  pdep = P.primPdep16
  {-# INLINE pdep #-}

instance Pdep Word32 where
  pdep = P.primPdep32
  {-# INLINE pdep #-}

instance Pdep Word64 where
  pdep = P.primPdep64
  {-# INLINE pdep #-}

-- | Runtime flag indicating whether the 'pdep' function is using the high-performance.
-- BMI2 instruction set.  A value of `False` indicates that `pdep` is emulated.
fastPdepEnabled :: Bool
fastPdepEnabled = P.fastPdepEnabled
{-# INLINE fastPdepEnabled #-}
