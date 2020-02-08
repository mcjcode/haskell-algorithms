module Polynomial
       (
        Polynomial,
        evalPoly,
        addPoly,
        subPoly,
        mulPoly,
        poly_xn
       ) where

type Polynomial a = [a]

evalPoly :: (Num a)
         => Polynomial a
         -> a
         -> a
evalPoly      [] x = 0
evalPoly (p0:ps) x = p0 + x * (evalPoly ps x)

-- | add two polynomials together
addPoly :: (Num a)
        => Polynomial a -- ^ the first summand p
        -> Polynomial a -- ^ the second summand q
        -> Polynomial a -- ^ the sum p+q
addPoly p1 [] = p1
addPoly [] p2 = p2
addPoly (p1:p1s) (p2:p2s) = (p1+p2):(addPoly p1s p2s)

-- | multiply two polynomials together
mulPoly :: (Num a)
        => Polynomial a -- ^ the first factor p
        -> Polynomial a -- ^ the second factor q
        -> Polynomial a -- ^ the product p*q
mulPoly [] p2 = []
mulPoly (p1:p1s) p2 = [p1*pp | pp<-p2] `addPoly` (0:(mulPoly p1s p2))

-- | subtract a second polynomial from the first two polynomials together
subPoly :: (Num a)
        => Polynomial a -- ^ the first polynomial p
        -> Polynomial a -- ^ the second polynomial q
        -> Polynomial a -- ^ the difference p-q
subPoly p1 [] = p1
subPoly [] p2 = p2
subPoly (p1:p1s) (p2:p2s) = (p1-p2):(subPoly p1s p2s)

poly_xn :: (Num a)
        => Int
        -> Polynomial a
poly_xn 0 = [1]
poly_xn n = 0:(poly_xn (n-1))
