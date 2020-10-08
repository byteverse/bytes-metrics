import Data.Bytes (Bytes)
import Data.Bytes.Metrics (levensteinWithTolerance)
import Data.List (unfoldr)
import Gauge.Main (defaultMain,bgroup,bench,whnf)
import System.Random (mkStdGen,uniformR)

import qualified Data.Bytes as Bytes


main :: IO ()
main = defaultMain
  [ bgroup "80-chars"
    [ bench "distance-1" (whnf (uncurry $ levensteinWithTolerance 1) strings)
    , bench "distance-2" (whnf (uncurry $ levensteinWithTolerance 2) strings)
    , bench "distance-3" (whnf (uncurry $ levensteinWithTolerance 3) strings)
    , bench "distance-4" (whnf (uncurry $ levensteinWithTolerance 4) strings)
    ]
  ]

prefix :: String
{-# noinline prefix #-} 
prefix = take 80 (unfoldr (Just . uniformR (' ', '~')) (mkStdGen 62861853071))

strings :: (Bytes, Bytes)
{-# noinline strings #-}
strings = (Bytes.fromAsciiString $ prefix ++ "a", Bytes.fromAsciiString $ prefix ++ "b")
