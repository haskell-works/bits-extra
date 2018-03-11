module Data.Bits.PextSpec (spec) where

import Data.Bits
import Data.Bits.Pext
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

slowPext64 :: Word64 -> Word64 -> Word64
slowPext64 = slowPext64' 0 0 0

slowPext32 :: Word32 -> Word32 -> Word32
slowPext32 s m = fromIntegral (slowPext64 (fromIntegral s) (fromIntegral m))

slowPext64' :: Word64 -> Int -> Int -> Word64 -> Word64 -> Word64
slowPext64' result offset index src mask = if index /= 64
  then if maskBit /= 0
          then slowPext64' nextResult (offset + 1) (index + 1) src mask
          else slowPext64' result      offset      (index + 1) src mask
  else result
  where srcBit      = (src  `shiftR` index) .&. 1
        maskBit     = (mask `shiftR` index) .&. 1
        nextResult  = result .|. (srcBit `shiftL` offset)

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

spec :: Spec
spec = describe "Data.Bits.PextSpec" $ do
  describe "Emulated pext and native pext give the same answer" $ do
    it "Word64" $ require $ property $ do
      a <- forAll $ G.word64 R.constantBounded
      b <- forAll $ G.word64 R.constantBounded
      pext a b === slowPext a b
    it "Word32" $ require $ property $ do
      a <- forAll $ G.word32 R.constantBounded
      b <- forAll $ G.word32 R.constantBounded
      pext a b === slowPext a b
    it "Word16" $ require $ property $ do
      a <- forAll $ G.word16 R.constantBounded
      b <- forAll $ G.word16 R.constantBounded
      pext a b === slowPext a b
    it "Word8" $ require $ property $ do
      a <- forAll $ G.word8 R.constantBounded
      b <- forAll $ G.word8 R.constantBounded
      pext a b === slowPext a b
    it "Word" $ require $ property $ do
      a <- forAll $ G.word R.constantBounded
      b <- forAll $ G.word R.constantBounded
      pext a b === slowPext a b
