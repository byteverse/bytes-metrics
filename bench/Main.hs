import Data.Bytes (Bytes)
import Data.Bytes.Metrics (levenshteinWithTolerance)
import Data.List (unfoldr)
import Gauge.Main (bench, bgroup, defaultMain, whnf)
import System.Random (mkStdGen, uniformR)

import qualified Data.Bytes as Bytes

main :: IO ()
main =
  defaultMain
    [ bgroup
        "80-chars"
        [ bench "distance-0" (whnf (uncurry $ levenshteinWithTolerance 0) strings)
        , bench "distance-1" (whnf (uncurry $ levenshteinWithTolerance 1) strings)
        , bench "distance-2" (whnf (uncurry $ levenshteinWithTolerance 2) strings)
        , bench "distance-3" (whnf (uncurry $ levenshteinWithTolerance 3) strings)
        , bench "distance-4" (whnf (uncurry $ levenshteinWithTolerance 4) strings)
        ]
    ]

prefix :: String
{-# NOINLINE prefix #-}
prefix = take 80 (unfoldr (Just . uniformR (' ', '~')) (mkStdGen 62861853071))

strings :: (Bytes, Bytes)
{-# NOINLINE strings #-}
strings = (Bytes.fromAsciiString $ prefix ++ "a", Bytes.fromAsciiString $ prefix ++ "b")
