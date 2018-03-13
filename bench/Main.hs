module Main where

import Criterion.Main
import Data.Bits
import Data.Bits.Pdep
import Data.Bits.Pext
import Data.Word

import qualified Data.Vector.Storable as DVS

mkVector :: (Num w, DVS.Storable w) => w -> IO (DVS.Vector w)
mkVector w = return $ DVS.unfoldrN 1024 gen w
  where gen b = let a = b + 1 in Just (a, a)

benchPopCount :: [Benchmark]
benchPopCount =
  [ env (mkVector (0 :: Word64)) $ \_ -> bgroup "popCount-single"
    [ bench "popCount64"  (whnf popCount (0 :: Word64))
    ]
  , env (mkVector (0 :: Word32)) $ \_ -> bgroup "popCount-single"
    [ bench "popCount32"  (whnf popCount (0 :: Word32))
    ]
  , env (mkVector (0 :: Word64)) $ \v -> bgroup "pdep-vector"
    [ bench "popCount64"  (whnf (DVS.map popCount) v)
    ]
  , env (mkVector (0 :: Word32)) $ \v -> bgroup "pdep-vector"
    [ bench "popCount32"  (whnf (DVS.map popCount) v)
    ]
  ]

benchPlus :: [Benchmark]
benchPlus =
  [ env (mkVector (0 :: Word64)) $ \v -> bgroup "plus-vector"
    [ bench "plus64"    (whnf (DVS.foldr (\a b -> (+) a 0 + b) 0) v)
    ]
  , env (mkVector (0 :: Word32)) $ \v -> bgroup "plus-vector"
    [ bench "plus32"    (whnf (DVS.foldr (\a b -> (+) a 0 + b) 0) v)
    ]
  ]

benchPdep :: [Benchmark]
benchPdep =
  [ env (mkVector (0 :: Word64)) $ \_ -> bgroup "pdep-single"
    [ bench "pdep64"    (whnf (pdep 0) (63 :: Word64))
    ]
  , env (mkVector (0 :: Word32)) $ \_ -> bgroup "pdep-single"
    [ bench "pdep32"    (whnf (pdep 0) (63 :: Word32))
    ]
  , env (mkVector (0 :: Word64)) $ \v -> bgroup "pdep-vector"
    [ bench "pdep64"    (whnf (DVS.foldr (\a b -> pdep a 0 + b) 0) v)
    ]
  , env (mkVector (0 :: Word32)) $ \v -> bgroup "pdep-vector"
    [ bench "pdep32"    (whnf (DVS.foldr (\a b -> pdep a 0 + b) 0) v)
    ]
  ]

benchPext :: [Benchmark]
benchPext =
  [ env (mkVector (0 :: Word64)) $ \_ -> bgroup "pext-single"
    [ bench "pext64"    (whnf (pext 0) (63 :: Word64))
    ]
  , env (mkVector (0 :: Word32)) $ \_ -> bgroup "pext-single"
    [ bench "pext32"    (whnf (pext 0) (63 :: Word64))
    ]
  , env (mkVector (0 :: Word64)) $ \v -> bgroup "pext-vector"
    [ bench "pext64"    (whnf (DVS.foldr (\a b -> pext a 0 + b) 0) v)
    ]
  , env (mkVector (0 :: Word32)) $ \v -> bgroup "pext-vector"
    [ bench "pext32"    (whnf (DVS.foldr (\a b -> pext a 0 + b) 0) v)
    ]
  ]

benchAll :: [Benchmark]
benchAll = benchPopCount <> benchPdep <> benchPext <> benchPlus

main :: IO ()
main = do
  putStrLn $ "Fast pdep enabled: " <> show fastPdepEnabled
  putStrLn $ "Fast pext enabled: " <> show fastPextEnabled
  defaultMain benchAll
