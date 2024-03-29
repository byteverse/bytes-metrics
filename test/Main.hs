{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Data.Bytes (Bytes)
import Data.Bytes.Metrics (levenshteinWithTolerance)
import Data.Word (Word8)

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck (Arbitrary (..), discard, testProperty, (===))

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Text.Ascii as Ascii
import qualified Data.Primitive as Prim
import qualified Test.Tasty.QuickCheck as TQC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ testGroup
        "sanity properties"
        [ testProperty "commutative" $ \a b ->
            let d = levenshteinWithTolerance 100 a b
                d' = levenshteinWithTolerance 100 b a
             in d === d'
        , testProperty "non-negative" $ \a b ->
            case levenshteinWithTolerance 100 a b of
              Nothing -> discard
              Just d -> d >= 0
        , testProperty "zero distance to self" $ \t a ->
            levenshteinWithTolerance (abs t) a a === Just 0
        , testProperty "distance to empty is length" $ \a ->
            let d = Bytes.length a
             in levenshteinWithTolerance d a Bytes.empty === Just d
        ]
    , testGroup
        "golden tests"
        [ testProperty "hellofworld" $
            let a = Ascii.fromString "hello world"
                b = Ascii.fromString "hellofworld"
             in levenshteinWithTolerance 10 a b == Just 1
        , testProperty "xyzzy" $
            let a = Ascii.fromString "xyzzy"
                b = Ascii.fromString "syzygy"
             in levenshteinWithTolerance 10 a b == Just 3
        ]
    ]

instance Arbitrary Bytes where
  arbitrary = do
    bs :: [Word8] <- TQC.arbitrary
    pure $ Bytes.fromByteArray $ Prim.byteArrayFromList bs
