
module Data.Bits.PdepSpec (spec) where

import Data.Bits
import Data.Bits.Pdep
import Data.Int
import Data.Word
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

slowPdep64 :: Word64 -> Word64 -> Word64
slowPdep64 = slowPdep64' 0

slowPdep32 :: Word32 -> Word32 -> Word32
slowPdep32 s m = fromIntegral (slowPdep64 (fromIntegral s) (fromIntegral m))

lsb :: Word64 -> Word64
lsb src = fromIntegral ((fromIntegral (src `shiftL` 63) :: Int64) `shiftR` 63)

slowPdep64' :: Word64 -> Word64 -> Word64 -> Word64
slowPdep64' result src mask = if lowest /= 0
  then slowPdep64' newResult (src `shiftR` 1) (mask .&. complement lowest)
  else result
  where lowest    = (-mask) .&. mask
        newResult = (result .|. ((lsb src) .&. lowest))

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

instance SlowPdep Int where
  slowPdep s m = fromIntegral (slowPdep64 (fromIntegral s) (fromIntegral m))

instance SlowPdep Int8 where
  slowPdep s m = fromIntegral (slowPdep64 (fromIntegral s) (fromIntegral m))

instance SlowPdep Int16 where
  slowPdep s m = fromIntegral (slowPdep64 (fromIntegral s) (fromIntegral m))

instance SlowPdep Int32 where
  slowPdep s m = fromIntegral (slowPdep64 (fromIntegral s) (fromIntegral m))

instance SlowPdep Int64 where
  slowPdep s m = fromIntegral (slowPdep64 (fromIntegral s) (fromIntegral m))

spec :: Spec
spec = describe "Data.Bits.PdepSpec" $ do
  describe "Emulated pdep and native pdep give the same answer" $ do
    it "Word64" $ require $ property $ do
      a <- forAll $ G.word64 R.constantBounded
      b <- forAll $ G.word64 R.constantBounded
      pdep a b === slowPdep a b
    it "Word32" $ require $ property $ do
      a <- forAll $ G.word32 R.constantBounded
      b <- forAll $ G.word32 R.constantBounded
      pdep a b === slowPdep a b
    it "Word16" $ require $ property $ do
      a <- forAll $ G.word16 R.constantBounded
      b <- forAll $ G.word16 R.constantBounded
      pdep a b === slowPdep a b
    it "Word8" $ require $ property $ do
      a <- forAll $ G.word8 R.constantBounded
      b <- forAll $ G.word8 R.constantBounded
      pdep a b === slowPdep a b
    it "Word" $ require $ property $ do
      a <- forAll $ G.word R.constantBounded
      b <- forAll $ G.word R.constantBounded
      pdep a b === slowPdep a b
