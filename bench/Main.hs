import Data.Bytes (Bytes)
import Data.Bytes.Metrics (levensteinWithTolerance)
import Data.List (unfoldr)
import Gauge.Main (defaultMain,bgroup,bench,whnf)
import System.Random (mkStdGen,uniformR)

import qualified Data.Bytes as Bytes


main :: IO ()
main = defaultMain
  [ bench "80 chars" $ whnf (uncurry $ levensteinWithTolerance 2) strings
  ]

prefix :: String
{-# noinline prefix #-} 
prefix = take 80 (unfoldr (Just . uniformR (' ', '~')) (mkStdGen 62861853071))

strings :: (Bytes, Bytes)
{-# noinline strings #-}
strings = (Bytes.fromAsciiString $ prefix ++ "a", Bytes.fromAsciiString $ prefix ++ "b")
