{-# LANGUAGE TemplateHaskell #-}

-- | General Utilities
module Utilities
    (
        digits,
        numFromDigits,
        fact,
        choose,
        desc_power,
        palindrome,
        combinations,
        comb,
        comb',
        countif,
        argmax,
        argmin,
        argmaxBy,
        argminBy,
        argminsBy,
        logBaseInt,
        deletei,
        merge_asc_streams,
        bps,
        bps2,
        bps2_w_sign,
        bps_w_rep,
        sum'
    ) where

import Data.List (minimum, maximum, subsequences, nub, sort, foldl')
import Test.QuickCheck (quickCheckAll)

sum' :: (Num a)
     => [a]
     -> a
sum' xs = foldl' (+) 0 xs

-- | delete the ith element of a list
deletei :: Int  -- ^ index of the element to delete
        -> [a]  -- ^ the list to delete from
        -> [a]
deletei i list = (take i list) ++ (drop (i+1) list)

-- | count the number of elements in a list for which a condition is true
countif :: (a -> Bool) -- ^ test for elements of list to pass/fail
        -> [a]         -- ^ the list of elements to test
        -> Integer     -- ^ the number of elements passing the test
countif q lst = foldl (\x y -> x + (if q y then 1::Integer else 0::Integer)) (0::Integer) lst

-- | return a list of the digits (little-endian) of a number in a given base
digits :: (Integral a)
       => a   -- ^ the base
       -> a   -- ^ the number n whose digits we want
       -> [a] -- ^ the digits of the number n
digits base 0 = []
digits base n = d:(digits base (div (n-d) base)) where d = mod n base 

-- | an inverse to 'digits', return the number
-- | corresponding to the given list of digits.
numFromDigits :: (Integral a)
              => a   -- ^ the base
              -> [a] -- ^ the digits (little-endian)
              -> a   -- ^ the number represented by the digits
numFromDigits base [] = 0
numFromDigits base (d:ds) = d + base*(numFromDigits base ds)

-- | factorial
fact :: (Integral a)
     => a -- ^ an integer n
     -> a -- ^ n!
fact 0 = 1
fact n = n * fact (n-1)

-- | product of k consecutive decreasing integers
-- | starting at n.
desc_power :: (Integral a)
           => a -- ^ n, an integer
           -> a -- ^ k, a non-negative integer 
           -> a -- ^ n*(n-1)*...*(n-k+1)
desc_power n k = dp 1 n k where
           dp acc n 0 = acc
           dp acc n k = dp (acc*n) (n-1) (k-1)

-- | n choose k
choose :: (Integral a)
       => a -- ^ n, a non-negative integer
       -> a -- ^ k, a non-negative integer
       -> a -- ^ nCk - the number of k-elt subsets of an n-elt set
choose n k = (desc_power n k) `div` (desc_power k k)

-- | is the given list a palindrome?
palindrome :: (Eq a)
           => [a]   -- ^ the list of elements
           -> Bool  -- ^ is it a palindrome?
palindrome list = list == reverse list

-- | return a list of all k-element subsets of
-- | the list [a..b]
comb' :: Int -- ^ the number of elements to choose
      -> Int -- ^ a - the beginning of the list
      -> Int -- ^ b - the end of the list
      -> [[Int]]  -- ^ the list of all k element subsets of [a..b]
comb' k a b
      | b-a+1 < k = []  -- can't take more elements than are left
      | k == 0    = [[]]
      | otherwise = (map (a:) $ comb' (k-1) (a+1) b) ++ (comb' k (a+1) b)

-- | return a list of all k-element subsets of a list
comb :: Int   -- ^ the number of elements to choose
     -> [a]   -- ^ the list to choose from.
     -> [[a]] -- ^ the list of k-elt subsets
comb k lst = map (map (lst!!)) $ comb' k 0 (length lst - 1)

-- | return a list of all k-element subsets of a list
combinations :: Int    -- ^ the number of elements to choose
             -> [a]    -- ^ the list to choose from.
             -> [[a]]  -- ^ the list of k-elt subsets
combinations 0 [] = [[]]
combinations k [] = []
combinations n (x:xs) = (map (x:) $ combinations (n-1) xs) ++ (combinations n xs)

-- | find the value x in a list where the function takes a maximum
argmaxBy :: (Ord a, Ord b)
         => (b -> a) -- ^ the function f
         -> [b]      -- ^ the list of inputs to f
         -> b        -- ^ an input where f is maximized 
argmaxBy f list = snd $ maximum [(f a, a) | a <- list]

-- | find the value x in a list where the function takes a minimum
argminBy :: (Ord a, Ord b)
         => (b -> a) -- ^ the function f
         -> [b]      -- ^ the list of inputs to f
         -> b        -- ^ an input where f is minimized 
argminBy f list = snd $ minimum [(f a, a) | a <- list]

-- | find the values xs in a list where a function takes a minimum
argminsBy :: (Ord a, Eq a)
          => (b -> a) -- the function f
          -> [b]      -- inputs for f
          -> [b]      -- the inputs where f is minimized
argminsBy f list = [x | x <- list, f x == minimum (map f list)]

-- | find the index i in a list where list !! i is maximized
argmax :: (Ord a)
       => [a]      -- ^ the list of inputs to f
       -> Int      -- ^ an input where f is maximized 
argmax list = argmaxBy (list!!) [0..(length list)-1]

-- | find the index i in a list where list !! i is minimized
argmin :: (Ord a)
       => [a]      -- ^ the list of inputs to f
       -> Int      -- ^ an input where f is maximized 
argmin list = argminBy (list!!) [0..(length list)-1]

-- | merge two ascending lists of elements
merge_2_asc_streams :: (Ord a)
                    => [a] -- ^ the first list
                    -> [a] -- ^ the second list
                    -> [a] -- ^ the merged list
merge_2_asc_streams [] xs2 = xs2
merge_2_asc_streams xs1 [] = xs1
merge_2_asc_streams (x1:x1s) (x2:x2s)
                  | x1 < x2 = x1:(merge_2_asc_streams x1s (x2:x2s))
                  | otherwise = x2:(merge_2_asc_streams (x1:x1s) x2s)

-- | merge all ascending lists of a list
merge_asc_streams :: (Ord a)
                  => [[a]] -- ^ the list of lists
                  -> [a]   -- ^ the merged list
merge_asc_streams (xx:xxs) = foldl merge_2_asc_streams xx xxs

-- | find the largest integral power of the base less than n
logBaseInt :: (Integral a1, Integral a2, Integral b)
           => a1 -- ^ the base b
           -> a2 -- ^ an integer n
           -> b  -- ^ the largest k s.t. b^k <= n
logBaseInt b n = floor $ logBase (fromIntegral b) (fromIntegral n)

prop_digits_numFromDigits_inverse1 n
                                   | n <= 0    = True
                                   | otherwise = n == numFromDigits 10 (digits 10 n)

-- | Given an upper bound, and an increasing list of factors
-- return the list of all lists of factors whose product
-- is no greater than the upper bound.  If you don't care
-- about the upper bound you can use subsequences in Data.List
bps :: Integral a
    => a      -- ^ the upper bound
    -> [a]    -- ^ the list of factors (possibly infinite)
    -> [[a]]
bps bd []       = [[]]
bps bd (x:xs)
    | x > bd    = [[]]
    | x*x > bd  = []:(map (:[]) $ takeWhile (<=bd) (x:xs))
    | otherwise = [x:ys | ys <- bps (bd`div`x) xs] ++ bps bd xs

-- | Given an upper bound, and an increasing list of factors
-- return the products of subsets of the factors no greater
-- than the upper bound.  Unlike bps, which produces lists of
-- factors, this produces lists of actual products.
bps2 :: Integral a
     => a      -- ^ the upper bound
     -> [a]    -- ^ the list of factors (possibly infinite)
     -> [a]    -- ^ the list of products
bps2 bd []       = [1]
bps2 bd (x:xs)
     | x > bd    = [1]
     | x*x > bd  = 1:(takeWhile (<=bd) (x:xs))
     | otherwise = [ x*y | y <- bps2 (bd`div`x) xs ] ++ bps2 bd xs

-- | Given an upper bound, and an increasing list of factors
-- return the products of subsets of the factors no greater
-- than the upper bound.  Unlike bps, which produces lists of
-- factors, this produces lists of actual products.
--
-- Also return (-1)^n where n is the number of factors used.
bps2_w_sign :: Integral a
     => a          -- ^ the upper bound
     -> [a]        -- ^ the list of factors (possibly infinite)
     -> [(Int,a)]  -- ^ the list of (signs,products)
bps2_w_sign bd []       = [(1,1)]
bps2_w_sign bd (x:xs)
     | x > bd    = [(1,1)]
     | x*x > bd  = (1,1):(map (\x -> (-1,x)) (takeWhile (<=bd) (x:xs)))
     | otherwise = [ (-sgn,x*y) | (sgn,y) <- bps2_w_sign (bd`div`x) xs ] ++ bps2_w_sign bd xs

-- | Given an upper bound, and an increasing list of factors
-- return the list of all products of these factors (with
-- repetition allowed) that are no greater than the upper bound.
bps_w_rep :: Integral a
          => a      -- ^ the upper bound
          -> [a]    -- ^ the increasing list of factors (possibly infinite)
          -> [a]    -- ^ the list of products
bps_w_rep bd []       = [1]
bps_w_rep bd (x:xs)
          | x > bd    = [1]
          | x*x > bd  = 1:(takeWhile (<=bd) (x:xs))
          | otherwise = [x*y | y <- bps_w_rep (bd`div`x) (x:xs)] ++ bps_w_rep bd xs

return []
runTests = $quickCheckAll
