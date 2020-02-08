module Combinatorics
       (
        npartitions,
        partitions,
        nchoosek_exponent,
        perm_order
       ) where

import Data.List (genericLength, genericIndex, (\\), foldl')

import Utilities (logBaseInt)

-- | return the nth pentagonal number
pent :: Int -- ^ the positive integer      = 1,2,3,4
     -> Int -- ^ the nth pentagonal number = 1,5,12,22
pent n = n * (3*n-1) `div` 2

wrap :: (Int -> Integer) -> Int -> Integer
wrap f x
    | x < 0 = 0
    | otherwise = f x

-- Generalized pentagonal numbers
gpent :: Int -> Int
gpent = (map p [0..] !!) where
    p n 
       | odd n     = pent ((n+1) `div` 2)
       | otherwise = pent (-(n   `div` 2))

gpsgn :: Int -> Integer
gpsgn i = if (i+1) `mod` 4 < 2 then (-1) else 1

-- | the number of partitions of a set of n identical things
-- memoized for reduced time complexity.  Uses Euler's
-- pentagonal number theorem.
--
-- n is an Int since n won't exceed maxBound::Int, but
-- #partitions(n) easily can, the output type is 'Integer'
--
npartitions :: Int      -- ^ The number of objects to be partitioned
            -> Integer  -- ^ The number of partitions
npartitions = let p n
                    | n==0      = 1
                    | otherwise = sum $ takeWhile (/=0) [(gpsgn i)*(npartitions (n - gpent i)) | i<-[1..]]
              in wrap (map p [0..] !!)

-- | return a list of all partitions of n
partitions :: Int     -- ^ a non-negative integer n
           -> [[Int]] -- ^ a list of all partitions of n (each in descending order) 
partitions n = p n n where
                 p 0 n = [[]]
                 p n 0 = []
                 p n k = [i:is | let mx = min n k, i <- [mx,mx-1..1], is <- p (n-i) (min i k)]

-- | from p_026.hs
untilRepeat :: (Eq b)
            => [b]      -- ^ the values already seen
            -> (a -> b) -- ^ the function
            -> [a]      -- ^ the inputs
            -> [b]      -- ^ the output values, until a repeat is seen
untilRepeat seen f (x:xs)
            | ((f x) `elem` seen) = ((f x):seen)
            | otherwise = untilRepeat ((f x):seen) f xs

cycle_at :: Integral a
         => a
         -> [a]
         -> [a]
cycle_at pos perm = reverse $ tail $ untilRepeat [] id (cycle_at' pos perm)

cycle_at' :: Integral a
          => a
          -> [a]
          -> [a]
cycle_at' pos perm = p:(cycle_at' p perm)
                     where p = (perm `genericIndex` pos)

cycles' :: (Integral a)
        => [a]   -- ^ the permutation
        -> [a]   -- ^ elements used so far
        -> [[a]] -- ^ the cycles of the permutation
cycles' used perm
        | next == []    = []
        | otherwise     = let cyc = (cycle_at (head next) perm) in cyc:(cycles' (used++cyc) perm) 
        where n = genericLength perm
              next = [0..n-1] \\ used

cycles :: (Integral a)
       => [a]
       -> [[a]]
cycles perm = cycles' [] perm

perm_order :: (Integral a)
           => [a]   -- ^ the permutation
           -> a     -- ^ the length
perm_order perm = foldl' (\x y -> lcm x (genericLength y)) 1 (cycles perm)

-- | return the exponent of the prime p in n_choose_k.
nchoosek_exponent :: Integral a
                  => a -- ^ n
                  -> a -- ^ k
                  -> a -- ^ p, a prime
                  -> a -- ^ the exponent of p appearing in n_choose_k
nchoosek_exponent n k p = sum [ div n pi - div k pi - div (n-k) pi | i <- [1..logBaseInt p n], let pi = p^i ] 

