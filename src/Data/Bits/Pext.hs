{-# LANGUAGE MagicHash #-}

module Data.Bits.Pext where

import GHC.Prim
import GHC.Word

class Pext a where
  pext :: a -> a -> a

instance Pext Word where
  pext (W#   src#) (W#   mask#) = W#   (pext#   src# mask#)

instance Pext Word8 where
  pext (W8#  src#) (W8#  mask#) = W8#  (pext8#  src# mask#)

instance Pext Word16 where
  pext (W16# src#) (W16# mask#) = W16# (pext16# src# mask#)

instance Pext Word32 where
  pext (W32# src#) (W32# mask#) = W32# (pext32# src# mask#)

instance Pext Word64 where
  pext (W64# src#) (W64# mask#) = W64# (pext64# src# mask#)
