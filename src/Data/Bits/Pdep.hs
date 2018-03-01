{-# LANGUAGE MagicHash #-}

module Data.Bits.Pdep where

import GHC.Prim
import GHC.Word

class Pdep a where
  pdep :: a -> a -> a

instance Pdep Word where
  pdep (W#   src#) (W#   mask#) = W#   (pdep#   src# mask#)

instance Pdep Word8 where
  pdep (W8#  src#) (W8#  mask#) = W8#  (pdep8#  src# mask#)

instance Pdep Word16 where
  pdep (W16# src#) (W16# mask#) = W16# (pdep16# src# mask#)

instance Pdep Word32 where
  pdep (W32# src#) (W32# mask#) = W32# (pdep32# src# mask#)

instance Pdep Word64 where
  pdep (W64# src#) (W64# mask#) = W64# (pdep64# src# mask#)
