{-# LANGUAGE TemplateHaskell #-}

module RowOperations where

import Matrix
import Data.List
-- import Test.QuickCheck

------------------------------------------------------------
-- Routines to carry out row and column reduction

vectorReduction :: (Num a, Fractional a) => Vector a -> Vector a -> Vector a
-- Performs the elementary row operation 
-- [ n, ... ] -> [ kn + q, ... ] -> [ q, ... ]
-- assuming that the first entry of the first vector is nonzero.
vectorReduction firstRow secondRow
  = vectorSum secondRow ( vectorScalarProduct (-k) firstRow )
  where
  k = (/) (head secondRow) (head firstRow)

nzFirst :: (Num a, Eq a) => Matrix a -> Matrix a
nzFirst [ ] = [ ]
nzFirst [row] = [row]
nzFirst (first:rest)
    | head first /= 0 = first:rest
    | otherwise       = (nzFirst rest) ++ [first]
    
-- minFirst :: (Num a, Ord a) => Matrix a -> Matrix a
-- -- Move the row whose first entry has the smallest absolute value
-- -- to be the first row. We must make sure the determinant is
-- -- multiplied by  -1  when we interchange rows. Also, since a
-- -- reversal of the row order will be needed at the end, we reverse
-- -- also at the start to ensure that the determinant is unchanged.
-- minFirst = reverse . minLast . reverse
--   where
--   minLast [ ] = [ ]
--   minLast [row] = [row]
--   minLast (first:second:rest)
--     | head first  == 0 = first : (minLast (second:rest))
--     | head second == 0 || abs (head first) < abs (head second)
--         = (vectorScalarProduct (-1) second) : (minLast (first:rest))
--     | otherwise        = first : (minLast (second:rest))

reduceUsingFirstRow :: (Num a, Eq a, Fractional a) => Matrix a -> Matrix a
reduceUsingFirstRow [ ] = [ ]
reduceUsingFirstRow m
  | head (head m) == 0 = m
  | otherwise = (head m) : [ vectorReduction (head m) row | row <- tail m ]

reduceFirstColumn :: (Num a, Eq a, Fractional a) => Matrix a -> Matrix a
-- Reduce as far as possible in the first column.
reduceFirstColumn m
  | onceReduced == m = m
  | otherwise = reduceFirstColumn (onceReduced)
  where
  onceReduced = reduceUsingFirstRow (nzFirst m)

tailBlock :: (Num a) => Matrix a -> Matrix a
-- A utility to pick out the lower right (n-1)-by-(n-1) block.
tailBlock m = [ tail row | row <- tail m ]

------------------------------------------------------------
-- Calculate the determinant by using row reductions 
-- to reduce to the upper triangular case

determinant :: (Num a, Eq a, Fractional a) => Matrix a -> a
determinant [ ]   = error "empty matrix has no determinant"
determinant [[n]] = n
determinant m 
  | numRows m /= numColumns m 
      = error "can only take determinant of square matrix"
  | otherwise 
      = (head (head reducedM) ) * (determinant ( tailBlock reducedM ))
  where
  reducedM = reduceFirstColumn m

------------------------------------------------------------
-- Calculate the inverse using row and column operations

doubleMatrix :: (Num a) => Matrix a -> Matrix a -> Matrix a
doubleMatrix = zipWith (++)

leftHalf :: (Num a) => Matrix a -> Matrix a
leftHalf double = [ take size row | row <- double ]
  where
  size = numRows double

rightHalf :: (Num a) => Matrix a -> Matrix a
rightHalf double = [ drop size row | row <- double ]
  where
  size = numRows double

upperTriangularize :: (Num a, Fractional a, Eq a) => Matrix a -> Matrix a
upperTriangularize [] = []
upperTriangularize m = [ head ( reduceFirstColumn m ) ] ++
  doubleMatrix (replicate (size-1) [0]) 
    (upperTriangularize ( tailBlock (reduceFirstColumn m)))
  where
  size = numRows m

matrixFlip :: Matrix a -> Matrix a
matrixFlip m = reverse [ reverse row | row <- m ]

doubleFlip :: (Num a) => Matrix a -> Matrix a
doubleFlip double 
  = doubleMatrix (matrixFlip ( leftHalf double )) ( matrixFlip ( rightHalf double ))

reduceLowerTriangular :: (Num a, Eq a, Fractional a) => Matrix a -> Matrix a
reduceLowerTriangular m =
  (take 1 m) ++ doubleMatrix (replicate ((numRows m)-1) [0]) 
  (reduceLowerTriangular (tailBlock (reduceUsingFirstRow m)))

-- not really fixing minus rows
fixMinusRows :: (Num a, Fractional a) => Matrix a -> Matrix a
fixMinusRows m = [ [ 1/((m!!i)!!i) * x | x <- m!!i ] | i <- [0..(numRows m)-1] ]

inverse :: (Num a, Eq a, Fractional a) => Matrix a -> Matrix a
inverse m = (rightHalf . fixMinusRows . doubleFlip . 
      reduceLowerTriangular . doubleFlip . upperTriangularize ) 
      ( doubleMatrix m ( idMatrix (numRows m) ) )

-- ------------------------------------------------------------
-- -- For Smith Normal Form, we reduce the first row and the first
-- -- column until  m  is in  1 + (n-1)  block form, then use recursion 
-- -- to peel off the diagonal entries

-- reduce' :: (Num a, Fractional a, Ord a) => Matrix a -> Matrix a
-- reduce' = transpose . reduceFirstColumn . transpose . reduceFirstColumn

-- reduce :: (Num a, Eq a, Fractional a, Ord a) => Matrix a -> Matrix a
-- -- When reduce' no longer decreases the (1,1) entry, the (1,1) entry 
-- -- divides the rest of the first column, and the rest of the first row 
-- -- is zero. So we just need one more reduction using the first row.
-- reduce m
--   | abs ( head (head (reduce' m)) ) == abs ( head (head m ) )
--       = reduceUsingFirstRow m
--   | otherwise        = reduce ( reduce' m )

prop_matrixFlip_involution :: (Num a, Eq a) => Matrix a -> Bool
prop_matrixFlip_involution mat = (matrixFlip . matrixFlip) mat == mat

-- return []
-- runTests = $quickCheckAll
