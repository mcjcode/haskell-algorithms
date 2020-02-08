-- Version of 10/28/06
module Matrix where

import Data.List
import Text.Printf (printf)

type Vector a = [a]
type Matrix a = [Vector a]

--basic constructions for vectors

zeroVector :: (Num a) => Int -> Vector a
zeroVector n = replicate n 0

--basic operations for vectors

dotProduct :: (Num a) => Vector a -> Vector a -> a
dotProduct v w = sum ( zipWith (*) v w )

vectorSum :: (Num a) => Vector a -> Vector a -> Vector a
vectorSum = zipWith (+)

vectorScalarProduct :: (Num a) => a -> Vector a -> Vector a
vectorScalarProduct n vec = [ n * x | x <- vec ]

--basic constructions for matrices

elemMatrix :: (Num a) => Int -> Int -> Int -> a -> Matrix a
-- elemMatrix n i j v   is the n-by-n elementary matrix with 
-- entry  v  in the (i,j) place
elemMatrix n i j v
  = [ [ entry row column | column <- [1..n] ] | row <- [1..n] ]
  where
  entry x y
    | x == y           = 1
    | x == i && y == j = v
    | otherwise        = 0

idMatrix :: (Num a) => Int -> Matrix a
idMatrix n = elemMatrix n 1 1 1

zeroMatrix :: (Num a) => Int -> Int -> Matrix a
zeroMatrix i j = replicate i (zeroVector j)

--basic operations for matrices

matrixSum :: (Num a) => Matrix a -> Matrix a -> Matrix a
matrixSum = zipWith vectorSum

matrixDifference :: (Num a) => Matrix a -> Matrix a -> Matrix a
matrixDifference = zipWith (zipWith (-))

matrixScalarProduct :: (Num a) => a -> Matrix a -> Matrix a
matrixScalarProduct n m = [ vectorScalarProduct n row | row <- m ]

matrixVectorProduct :: (Num a) => Matrix a -> Vector a -> Vector a
matrixVectorProduct m v = [ dotProduct row v | row <- m]

matrixProduct :: (Num a) => Matrix a -> Matrix a -> Matrix a
matrixProduct m n = [ map (dotProduct r) (transpose n) | r <- m ]

modularMatrixProduct :: (Num a, Integral a) => a -> Matrix a -> Matrix a -> Matrix a
modularMatrixProduct modulus m n = [ map ((`mod`modulus).(dotProduct r)) (transpose n) | r <- m ]

matrixPower m 0 = idMatrix (numRows m)
matrixPower m 1 = m
matrixPower m n
            | n `mod` 2 == 0   = matrixProduct (matrixPower m (n`div`2)) (matrixPower m (n`div`2))
            | otherwise        = matrixProduct (matrixPower m (n-1)) m

-- determinant using cofactors

remove :: (Num a) => Matrix a -> Int -> Int -> Matrix a
remove m i j  
  | (length m == 0) || i < 1 || i > numRows m || j < 1 || j > numColumns m
    = error "(i,j) out of range"
  | otherwise = transpose ( cut (transpose ( cut m i ) ) j )

determinant' :: (Num a) => Matrix a -> a
determinant' [] = error "determinant: 0-by-0 matrix"
determinant' [[n]] = n
determinant' m = sum [ (-1)^(j+1) * (head m)!!(j-1) * determinant' (remove m 1 j) | 
  j <- [1..(numColumns m) ] ]

cofactor' :: (Num a) => Matrix a -> Int -> Int -> a
cofactor' m i j = (-1)^(i+j) * determinant' (remove m i j)

cofactorMatrix' :: (Num a) => Matrix a -> Matrix a
cofactorMatrix' m = [ [ (cofactor' m i j) | j <- [1..n] ] | i <- [1..n] ]
  where
  n = length m

inverse' :: (Num a, Fractional a) => Matrix a -> Matrix a
inverse' m = transpose [ [ (/) x ( determinant' m) | 
  x <- row ] | row <- (cofactorMatrix' m) ]

----------------------------------------------------------
-- matrix utilities

numRows :: (Num a)
        => Matrix a -- ^ the matrix
        -> Int      -- ^ the number of rows
numRows = length

numColumns :: (Num a)
           => Matrix a -- ^ the matrix
           -> Int      -- ^ the number of columns
numColumns = length . head

matrixConcat :: (Num a)
             => Matrix a
             -> Matrix a
             -> Matrix a
matrixConcat [m1] [m2] = [m1++m2]
matrixConcat (m1:m1s) (m2:m2s) = (m1++m2):(matrixConcat m1s m2s)

directSum :: (Num a)
          => Matrix a
          -> Matrix a
          -> Matrix a
directSum m1 m2 = (matrixConcat m1 (zeroMatrix (numRows m1) (numColumns m2))) ++
                  (matrixConcat (zeroMatrix (numRows m2) (numColumns m1)) m2)

----------------------------------------------------------
-- other utilities

cut :: [a] -> Int -> [a]
cut [] n = []
cut xs n
  | n < 1 || n > (length xs) = xs
  | otherwise = (take (n-1) xs) ++ drop n xs

-- showMatrix :: (Num a) => Matrix a -> IO()
-- showMatrix m = putStr (rowsWithEndlines m)
--   where
--   rowsWithEndlines m = concat ( map (\x -> (show x) ++ "\n") m )

----------------------------------------------------------
-- test data

mat1, mat2, mat3, mat4, mat5, mat6, mat7, mat8, mat9, mat10, 
  mat11, mat12, mat13 :: (Num a) => Matrix a
mat1 = [ [1,2,3], [4,5,6], [7,8,9] ]
mat2 = [ [1,2,3], [4,5,6], [7,8,10] ]
mat3 = [ [1,2,3, 0], [-6, 4,5,6], [7,8,10, 10], [-3, -2, -1, 4] ]
mat4 = [ [1,2], [1,1]]
mat5 = [ [1, 1, 0], [0, 1, 0], [0, 0, 1] ]
mat6 = [[1,2,0, 3,0],[-6,4,0, 5,6],[7,8,72, 10,10],[-3,-2,-1,4, -103]]
mat7 = [[2,0, 3,0],[-4,0, 5,6],[8,72, 10,10],[-2,-1,4, -103],[8,9,0,-14]]
mat8 =[ [0,0,0], [0,0,0], [0,0,0]]
mat9 = [ [2, 0], [0, 1] ]
mat10 = [ [1, 0], [ 0, 2] ]
mat11 = [ [2, 0, 0], [0, 3, 0], [0, 0, 1] ]
mat12 = [[0,0,0,0,0],[1,1,-1,-1,0],[0,0,1,1,-1],[0,1,-1,-1,0],[1,0,0,1,-1]]
mat13 = [[-1,0,0,-1,0,0,0,-1,0,0,0,-1],[1,0,1,1,1,1,1,1,1,1,1,1],[-1,0,-1,0,-1,0,0,0,-1,0,0,0],[0,-1,0,-1,0,-1,0,0,0,-1,0,0],[0,0,0,1,-1,0,0,0,0,0,0,0],[-1,-1,-1,-1,0,-1,0,0,0,0,0,0],[1,0,0,0,0,0,-1,0,0,0,0,0],[0,1,0,0,0,0,0,-1,0,0,0,0],[0,0,0,0,0,0,0,1,-1,0,0,0],[0,0,0,0,-1,-1,-1,-1,0,-1,0,0],[0,0,0,0,1,0,0,0,0,0,-1,0],[0,0,0,0,0,1,0,0,0,0,0,-1]]
----------------------------------------------------------
{-
-- test of the inverse routine

eProduct :: Int -> [(Int,Int,Int)] -> Matrix
-- eProduct n [(Int,Int,Int)] is the n-by-n matrix which is a product
-- of the elementary n-by-n matrices given by the list triples
eProduct n [] = idMatrix n
eProduct n ((i,j,v):rest) = matrixProduct ( elemMatrix n i j v) (eProduct n rest)

minSize :: [(Int,Int,Int)] -> Int
-- smallest size of matrix for which all elementary matrices are defined
minSize list = maximum (concat [ [i,j] | (i,j,value) <- list ] )

checkInverse :: [(Int,Int,Int)] -> String
checkInverse list =
  "\n   M       = " ++ (show m) ++ 
  "\nInverse(M) = " ++ (show (inverse m)) ++ 
  if matrixProduct m (inverse m) == idMatrix n then "\nOK.\n" else "\nError.\n"
  where
  m = eProduct n list
  n = minSize list

list1 :: [(Int,Int,Int)]
list1 =  [(1,2,1), (1,3,-1), (1,2,1), (3,2,-2), (3,1,-3)]

list2 :: [(Int,Int,Int)]
list2 =  [(1,2,4), (4,2,-1), (4,1,-2), (4,1,1), (1,3,-3), (2,3,2), 
  (1,2,2), (1,4,-3), (1,3,-1), (3,2,-1), (3,1,-1)]

test :: IO()
test = putStr ( 
  ( checkInverse list1 ) ++ 
  ( checkInverse list2 )
  )

----------------------------------------------------------

--Here is the output of the test routine:

Main> test

[[1,-2,1],[0,1,0],[3,-4,4]]
[[1,-2,1],[0,1,0],[3,-4,4]]
Equal.


[[4,-15,8,3],[0,1,-2,0],[4,-14,7,3],[1,-3,0,1]]
[[4,-15,8,3],[0,1,-2,0],[4,-14,7,3],[1,-3,0,1]]
Equal.

-}




