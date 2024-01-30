{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Bytes.Metrics
  ( levenshteinWithTolerance
  , isWithinLevenshtein
  ) where

import Control.Monad.ST (runST)
import Data.Bytes (Bytes)

import qualified Data.Bytes as Bytes
import qualified Data.Primitive.Contiguous as Arr
import qualified Data.Primitive.PrimArray as Prim

{- | Determine if two 'Bytes' are within a given Levenshtein distance of each other (inclusive).
Computes in O(t*min(n,m)) time and O(min(t,n,m)) space,
where @n,m@ are lengths of the input strings and @t@ is the tolerance.
-}
isWithinLevenshtein :: Int -> Bytes -> Bytes -> Bool
isWithinLevenshtein t a b = maybe False (<= t) $ levenshteinWithTolerance t a b

{- | Determine Levenshtein distance between two strings, as long as their
distance is within (inclusive) the given tolerance.
Computes in O(t*min(n,m)) time and O(min(t,n,m)) space,
where @n,m@ are lengths of the input strings and @t@ is the tolerance.
-}
levenshteinWithTolerance :: Int -> Bytes -> Bytes -> Maybe Int
levenshteinWithTolerance !t !a !b
  -- ensure that the first string (which will create columns) is longer
  -- this minimizes the space needed for intermediate results
  | t == 0 = if a == b then Just 0 else Nothing
  | m > n = levenshteinWithWorker t b a
  | otherwise = levenshteinWithWorker t a b
 where
  m = Bytes.length a
  n = Bytes.length b

-- Precondition: Length of A is less than or equal to length of B.
levenshteinWithWorker :: Int -> Bytes -> Bytes -> Maybe Int
levenshteinWithWorker !t !a !b
  | t < deltaN = Nothing
  | otherwise = runST $ do
      -- during table creation, some column indices will be negative:
      -- the contents of such oob cells must not impact the contents of in-bounds cells
      -- using maxBound to initialize could provoke overflow on increment
      -- using n+m will definitely be larger than any entry in the table, but likely small enough to avoid wrapping arithmetic
      row :: Prim.MutablePrimArray s Int <- Arr.replicateMut rowLen (n + m)
      let outerLoop !rowIx
            | rowIx <= m = do
                let innerLoop !bandIx
                      | bandIx < rowLen = do
                          let colIx = rowIx - p + bandIx
                          let initCost = if rowIx == 0 && colIx == 0 then 0 else maxBound
                          let !byteA = Bytes.unsafeIndex a (rowIx - 1)
                          let !byteB = Bytes.unsafeIndex b (colIx - 1)
                          !editCost <-
                            if
                              | not (1 <= colIx && colIx <= n) -> pure maxBound
                              | byteA == byteB -> Arr.read row bandIx
                              | otherwise -> (1 +) <$> Arr.read row bandIx
                          !insCost <-
                            if 0 <= bandIx - 1
                              then (1 +) <$> Arr.read row (bandIx - 1)
                              else pure maxBound
                          !delCost <-
                            if bandIx + 1 < rowLen
                              then (1 +) <$> Arr.read row (bandIx + 1)
                              else pure maxBound
                          let cost = min (min initCost editCost) (min insCost delCost)
                          Arr.write row bandIx cost
                          innerLoop (bandIx + 1)
                      | otherwise = pure ()
                innerLoop 0
                outerLoop (rowIx + 1)
            | otherwise = pure ()
      outerLoop 0
      d <- Arr.read row (deltaN + p)
      pure $ Just d
 where
  m = Bytes.length a
  n = Bytes.length b
  deltaN = n - m
  -- FIXME what a gross name, what even is p really supposed to be? a one-sided external tolerance for the diagonal band?
  p = (t - deltaN) `quot` 2
  -- \| the other way to think of this length is `t - deltaN + (1 - t `mod` 2)`
  -- the floor operation to compute `p` is what gives it that awful last term, and why I'm sticking with the paper's presentation
  rowLen = 1 + deltaN + 2 * p
