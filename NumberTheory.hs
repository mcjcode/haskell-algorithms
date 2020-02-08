-- | Number Theory functions
module NumberTheory
    (
        -- * Factorization functions
        smallest_factor,
        pexponent,
        pexponent_and_quotient,
        factorize,
        factorize_int,
        fast_div_cnt,
        factorization_to_int,
        factorizations,
        factors,
        smallest_factor_power,

        -- * multiplicative functions
        multiplicative,
        divisor_function,
        divisor_function_primary,
        totient_function,

        -- * summatory multiplicative functions
        --summatory_totient_function,
        
        -- * primality testing
        isprime,
        isprime_miller_rabin,
        isprimeint,

        -- * mod-n functions
        order,
        primitive_root,
        mod_pow,

        -- * pell's equation
        cont_frac_rep,
        pell,

        -- * prime counting functions
        nprimes,
        num_primes_legendre,
        num_primes_legendre_have_primes,
        num_primes_meissel,

        -- * squares and cubes
        squarefree_ints_factorized,
        issq,
        issquarefree,
        sqrtInt,
        cbrtInt,

        -- * fibonacci related functions
        fibonacci,
        
        -- * other miscelaneous functions
        euclidean_alg,
        chinese_remainder_theorem,
        kgonal,
        padic_valuation,
        divides,


    ) where

import Debug.Trace

import Data.List
import Data.Ratio
import Test.QuickCheck

import Utilities (countif, bps2_w_sign, sum')
import qualified Primes

-- | taking powers in modular arithmetic
mod_pow :: (Integral a)
        => a -- ^ modulus m
        -> a -- ^ exponent e
        -> a -- ^ base b
        -> a -- ^ b^e mod m
mod_pow md 0 k = 1
mod_pow md 2 k = (k*k) `mod` md
mod_pow md p k
        | even p = let kp2 = (mod_pow md (p `div` 2) k) in mod_pow md 2 kp2
        | odd p  = (k * (mod_pow md (p-1) k)) `mod` md

mod_powers md a = mp md a 1 where
          mp md a a0 = a0:(mp md a ((a*a0) `mod` md))

enumerate = zip [0..]

-- | return the order of the second argument
-- | in the multiplicative group of non-zero
-- | residue classes mod the first argument
order :: Integral b
      => b      -- ^ the modulus md
      -> b      -- ^ an integer a relatively prime to md
      -> Integer -- ^ the smallest positive power of a congruent to 1 mod md
order md a = fst $ head $ filter ((==1).snd) $ drop 1 $ enumerate (mod_powers md a)

-- | pseudoprimality test using fermat's theorem
ispseudoprime :: (Integral a)
              => a -- ^ modulus (should be a prime)
              -> a -- ^ integer to be tested
              -> Bool -- ^ is the integer a pseudoprime in this modulus
ispseudoprime p n
              | n == p    = True
              | otherwise = mod_pow n (n-1) p == 1

-- | return true iff the predicate fails for all elements of the list
none :: (a -> Bool) -- ^ the condition 
    -> [a]        -- ^ the collection
    -> Bool        -- ^ does the condition fail for all elements of the collection
none _    []                  = True
none pred (x : xs) | pred x    = False
                  | otherwise = none pred xs
                  
-- | primality test using trial division
isprime :: (Integral a)
        => a    -- ^ integer to be tested
        -> Bool -- ^ is the integer a prime
isprime 0 = False
isprime 1 = False
isprime 2 = True
isprime n = none (`divides` n) $ [2..sqrtIntUp (abs n)]

-- | probabilistic Miller-Rabin primality test
isprime_miller_rabin :: (Integral a)
                    => a
                    -> Bool
isprime_miller_rabin n | n <= 1      = False
                      | n == 2      = True
                      | n`mod`2 == 0 = False
                      | otherwise    = not $ has_miller_rabin_witness n

has_miller_rabin_witness :: (Integral a)
                        => a
                        -> Bool
has_miller_rabin_witness n = or (map (is_miller_rabin_witness n) $ filter (<n) [2,3,5,7])
-- has_miller_rabin_witness n = or [is_miller_rabin_witness a n | a <- filter (<n) [2,3,5,7]]

-- | is the second argument a Miller-Rabin 'strong witness'
-- for the compositeness of the first argument?
--
-- If the result is true, compositeness is proved.  If false,
-- then we are more certain of primality, but this is not a proof
is_miller_rabin_witness :: (Integral a)
                        => a    -- ^ the number whose primality is at issue
                        -> a    -- ^ the potential witness to be tested
                        -> Bool -- ^ does the potential witness prove compositeness
is_miller_rabin_witness n a = (ad /= 1) && (and $ map (/=(n-1)) $ genericTake s ads)
                        where (s,d) = pexponent_and_quotient 2 (n-1)
                              ad = mod_pow n d a
                              ads = ad:(map (\x -> (x*x)`mod`n) ads)

-- | Return 1 if argument is prime, 0 otherwise
isprimeint :: (Integral a)
          => a  -- ^ the integer to be tested
          -> Int -- ^ 1 if it is prime, 1 otherwise.
isprimeint n
          | isprime n = 1
          | otherwise = 0

-- | Return the number of primes between 1 and n
nprimes :: (Integral a)
        => a      -- ^ the upper bound of integers to test
        -> Integer -- ^ the number of primes between 1 and the upper bound.
nprimes n = countif isprime [1..n]

primes :: Integral a
      => [a]
primes = filter isprime [2..]

-- | find the smallest (>1) factor of an integer
smallest_factor :: (Integral a)
                => a -- ^ an integer n
                -> a -- ^ n's smallest (>1) factor
smallest_factor n = head pp
                    where sqrtn = sqrtInt n
                          pp = (filter (`divides`n) $ takeWhile (<=sqrtn) Primes.primes) ++ [n]

-- | is the first integer a factor of the second?
divides :: (Integral a)
        => a -- ^ the proposed factor d
        -> a -- ^ the proposed multiple n
        -> Bool -- ^ is d a factor of n?  (does d divide n evenly)
divides d n = n `mod` d == 0

-- | return the number of times an integer divides another
pexponent :: (Integral a)
          => a -- ^ the proposed factor d
          -> a -- ^ the proposed multiple n
          -> a -- ^ the number of times d divides n
pexponent d n
          | d `divides` n  = pexponent d (n `div` d) + 1
          | otherwise      = 0

-- | return 'e' the number of times the first
-- argument 'd' divides the second argument 'n',
-- and 'q' the quotient that remains.
--
-- \( n = d^e * q \), where \( d \) does not divide \( q \).
--
pexponent_and_quotient :: (Integral a)
                      => a -- ^ the proposed factor d
                      -> a -- ^ the proposed multiple n
                      -> (a,a) -- ^ the exponent e of d in 
pexponent_and_quotient d n
                      | r == 0    = let (exp,quot) = pexponent_and_quotient d q in (exp+1,quot)
                      | otherwise = (0,n)
                      where (q,r) = n `divMod` d

-- | find the number of times an integer's smallest factor appears in its prime factorization.
smallest_factor_power :: (Integral a)
                      => a -- ^ an integer n
                      -> (a,a) -- ^ the smallest factor and its exponent in n 
smallest_factor_power n = (p,pexponent p n) where p=smallest_factor n

-- | return the prime factorization of an integer
factorize :: (Integral a)
          => a -- ^ the number to be factored
          -> ([a],[a]) -- ^ the primes in the factorization, and their exponents, in two separate lists
factorize 1 = ([],[])
factorize n = (p:ps,e:es)
          where p = smallest_factor n
                e = pexponent p n
                (ps,es) = factorize (n `div` (p^e))

factorize_int :: Int
              -> ([Int],[Int])
factorize_int 1 = ([],[])
factorize_int n = (p:ps,e:es)
              where p = smallest_factor n
                    e = pexponent p n
                    (ps,es) = factorize_int (n `div` (p^e))
                
-- | return the exponents in the prime factorization of an integer
fast_div_cnt :: Int -- ^ the number to be factored
            -> Int -- ^ the primes in the factorization, and their exponents, in two separate lists
fast_div_cnt 1 = 1
fast_div_cnt n = (e+1) * fast_div_cnt (n`div`(p^e))
                where p = smallest_factor n
                      e = pexponent p n

-- | return the largest integer whose square does not exceed n
sqrtInt :: (Integral a)
        => a -- ^ the integer being sqrt-ed
        -> a -- ^ the square-root
sqrtInt = floor . sqrt . fromIntegral

-- | is the integer a square?
issq :: (Integral a)
    => a
    -> Bool
issq n = (sqrtInt n)^2 == n

-- | return the largest integer whose cube does not exceed n
cbrtInt :: (Integral a)
        => a -- ^ the integer being cube-rooted
        -> a -- ^ the cube root
cbrtInt 0 = 0
cbrtInt n = (floor . (**(1/3)) . fromIntegral) n

-- | return the least integer whose square is not less than n
sqrtIntUp :: (Integral a)
          => a -- ^ the integer being sqrt-ed
          -> a -- ^ the sqrt
sqrtIntUp = ceiling . sqrt . fromIntegral

-- | return the list of all factors of an integer, in increasing size
factors :: (Integral a)
        => a -- ^ the integer n whose factors we seek
        -> [a] -- ^ a list of factors of \( n \)
factors n = sort $ factors2 n

-- here is the older, usually slower, implementation
-- that just uses trial division through \( \sqrt{n} \).
--
-- factors n = facts ++ (reverse (map (n`div`) (filter ((/=n).(^2)) facts)))
--            where facts = filter (`divides`n) [1..sqrtInt n]

-- | return the list of all factors of an integer, not
-- necessarily in order of size (but faster than just
-- trial division through \( \sqrt{n} \)
factors2 :: (Integral a)
        => a -- ^ the integer \( n \) whose factors we seek
        -> [a] -- ^ a list of the factors of \( n \)
factors2 n | n == 0    = []
          | n == 1    = [1]
          | otherwise = [ p^ee * f | f <- facts', ee <- [0..e] ] 
                      where p = smallest_factor n
                            e = pexponent p n
                            facts' = factors2 $ n`div`(p^e)
                      
-- | produce a list of all factorizations of an integer
factorizations :: (Integral a)
              => a -- ^ the integer n to be factored
              -> [[a]] -- ^ a list of factorizations of n
factorizations n = f n 1 where
              f 1 k = [[]]
              f n k = [ d:ds | d <- filter (>=k) $ filter (/=1) $ factors n, ds <- f (n`div`d) (max k d) ] 

-- | given its factorization (primes and exponents), return the integer
factorization_to_int :: (Integral a)
                    => [a] -- ^ the primes p1,p2,..
                    -> [a] -- ^ their exponents e1,e2,..
                    -> a  -- ^ the integer p1^e1 * p2^e2 * ...
factorization_to_int ps es = product [p^e | (p,e) <- zip ps es]

-- | for a prime power p^e, return the sum of the kth powers of the divisors
divisor_function_primary :: (Integral a)
                        => a -- ^ the power k
                        -> a -- ^ a prime
                        -> a -- ^ the exponent
                        -> a -- ^ the sum of the kth powers of divisors of p^e
--divisor_function_primary k p e = sum [(p^k)^j | j <- [0..e]]
divisor_function_primary 0 p e = e+1
divisor_function_primary k p e = ((p^k)^(e+1)-1)`div`(p^k-1)

-- | if f is a multiplicative function, compute it by computing it on relatively
-- | prime factors and taking the product.
multiplicative :: (Integral a, Num b)
              => (a -> a -> b) -- ^ the function f of two arguments (a prime p and its exponent e)
              -> a            -- ^ the integer at which to evaluate the f
              -> b            -- ^ f(n)
multiplicative f n = product [f p e | (p,e) <- zip ps es] where (ps,es) = factorize n

-- | for any integer, return the sum of the kth powers of the divisors
divisor_function :: (Integral a)
                => a -- ^ the power k
                -> a -- ^ the integer n whose divisors are being summed over
                -> a -- ^ the sum of the kth powers of the divisors
divisor_function k = multiplicative (divisor_function_primary k)

-- | for prime p and exponent e, the number of residue classes mod p^e relatively prime to p^e
totient_function_primary :: (Integral a)
                        => a -- ^ the prime p
                        -> a -- ^ the exponent
                        -> a -- ^ the number of residue classes mod p^e relatively prime to p^e
totient_function_primary p i = p^(i-1) * (p-1)

-- | for an integer n, the number of residue classes mod n relatively prime to n
totient_function :: (Integral a)
                => a -- ^ the modulus
                -> a -- ^ the number of residue classes relatively prime to the modulus
totient_function = multiplicative totient_function_primary

summatory_totient_function_simple :: (Integral a)
                                  => a -- ^ an integer n
                                  -> a -- ^ \( \phi(1) + \cdots + \phi(n) \)
summatory_totient_function_simple n = sum $ map totient_function [1..n]

summatory_totient_function' :: (Integral a)
                            => a -- ^ an integer n
                            -> a -- ^ \( \phi(1) + \cdots + \phi(n) \)
summatory_totient_function' 0 = 0
summatory_totient_function' n = ((n*(n+1))`div`2) - (part1 + part2)
                          where m = sqrtInt n
                                part1 = sum $ [ summatory_totient_function' (n`div`k) | k <- [2..m]]
                                part2 = sum $ [ (summatory_totient_function' k)*((n`div`k)-(n`div`(k+1))) | k <- [1..m], k*(k+1) <= n]

-- | given an integer n, return the sum of the
-- Euler totient function at the integers 1..n
summatory_totient_function'' :: Int -- ^ an integer n
                            -> Int -- ^ \( \phi(1) + \cdots + \phi(n) \)
summatory_totient_function'' = (map p [0..] !!) where p 0 = 0
                                                      p n = ((n*(n+1))`div`2) - (part1 + part2)
                                                        where m = sqrtInt n
                                                              part1 = sum $ [ summatory_totient_function'' (n`div`k) | k <- [2..m]]
                                                              part2 = sum $ [ (summatory_totient_function'' k)*((n`div`k)-(n`div`(k+1))) | k <- [1..m], k*(k+1) <= n]



-- | given an integer n, return the sum of the
-- Euler totient function at the integers 1..n
--
-- Look at the source code to see the pattern
-- for memoizing a function defined for natural
-- numbers (nats).
--summatory_totient_function :: Integer -> Integer
--summatory_totient_function = index $ fmap (f summatory_totient_function) nats
--                          where f mf 0 = 0
--                                f mf n = ((n*(n+1))`div`2) - (part1 + part2)
--                                  where m = sqrtInt n
--                                        part1 = sum $ [ mf (n`div`k) | k <- [2..m]]
--                                        part2 = sum $ [ (mf k)*((n`div`k)-(n`div`(k+1))) | k <- [1..m], k*(k+1) <= n]
--

-- | produce kgonal numbers
kgonal :: (Integral a)
      => a -- ^ the shape k of the numbers (k=4 is square)
      -> a -- ^ the position n in the sequence (n=1 gives 1, n=2 gives k)
      -> a -- ^ the nth kgonal number
kgonal k n = (k-2)*n*(n-1) `div` 2 + n

-- | return the least common multiple
-- | (1 if the foldable is empty)
--lcm_fold :: (Foldable t, Integral b)
--        => t b -- ^ a foldable of integral elements (e.g. a [Int])
--        -> b  -- ^ the least common multiple of the elements of the foldable
--lcm_fold = foldl lcm 1

-- | the euclidean algorithm for finding gcds
-- and keeping track of how to express the
-- gcd as a integral combination of the
-- starting numbers
euclidean_alg :: Integral a
              => a      -- ^ the first int m
              -> a      -- ^ the second int n
              -> (a,a,a) -- ^ the gcd g and (a,b) s.t. g = a*m+b*n
euclidean_alg m n = mygcd' m (1,0) n (0,1)
                    where mygcd' m (m1,m2) n (n1,n2)
                                | m == 0    = (n,n1,n2)
                                | otherwise = mygcd' r (n1-d*m1,n2-d*m2) m (m1,m2)
                                where (d,r) = divMod n m 

-- | Chinese remainder theorem
chinese_remainder_theorem :: (Integral a)
    => a -- ^ first modulus  mod1
    -> a -- ^ first residue
    -> a -- ^ second modulus mod2
    -> a -- ^ second residue
    -> a -- ^ the residue modulus mod1*mod2 congruent to res1 mod mod1 and res2 mod mod2
chinese_remainder_theorem m1 r1 m2 r2 = (r2*c1*m1+r1*c2*m2) `mod` (m1*m2)
                                        where (_,c1,c2) = euclidean_alg m1 m2

prop_factors_of_primes :: (Integral a) => a -> Bool
prop_factors_of_primes n
                      | n < 1        = True
                      | isprime n    = (factors n) == [1,n]
                      | n == 1        = (factors n) == [1]
                      | otherwise    = length (factors n) > 2

padic_valuation :: (Integral a)
                => a        -- ^ the base of the valuation (a prime, or zero)
                -> Ratio a  -- ^ the rational number to value
                -> Ratio a  -- ^ the valuation
padic_valuation p x
                | p == 0    = abs x
                | otherwise = let num = numerator x
                                  den = denominator x
                                  en = (pexponent p num)
                                  ed = (pexponent p den)
                              in (p^ed) % (p^en)

ff :: Integral a
  => a          -- ^ the prime to apply to the second argument
  -> [(a,[a],a)] -- ^ the sequence p is applied to
  -> [(a,[a],a)] -- ^ the resulting sequence
ff p [] = []
ff p ((rn,pn,nn):rpns) = (if pi == 0 then (rn,pn,nn) else (if pi == 1 then (rn*p,(p:pn),nn) else (0,pn,nn))):(ff p rpns)
                        where pi = pexponent p nn

primes_below_n13 n = takeWhile (<(cbrtInt n)) primes

-- | return the factorizations of all of the square free
-- | integers strictly less than an upper bound.
squarefree_ints_factorized :: Integral a
                          => a      -- ^ the upper bound n
                          -> [[a]]  -- ^ the list of factorizations of sq free integers < n
squarefree_ints_factorized ub = map (\(rn,pn,nn) -> (if (rn/=nn) then ((nn`div`rn):pn) else pn)) $ filter (\(a,b,c)->a/=0) (squarefree_ints2 ub)

squarefree_ints2 ub = squarefree_ints' ub (takeWhile ((<ub).(^2)) primes) [(1,[],nn) | nn <- [1..(ub-1)]]
                where squarefree_ints' ub [] so_far =  so_far
                      squarefree_ints' ub (p:ps) so_far = squarefree_ints' ub ps (ff p so_far)

-- | return whether the argument is squarefree
issquarefree :: Integral a
            => a    -- ^ an integer n
            -> Bool  -- ^ is n squarefree
issquarefree n
            | n == 1    = True -- handle 1 separately, since factorize returns ([],[])
            | otherwise = ((==1) . maximum . snd . factorize) n -- no prime occurs to a power more than 1

toReal (a,b,c,n) = ((fromIntegral a) + (fromIntegral b)*(sqrt (fromIntegral n)))/(fromIntegral c)

reduce (a,b,c,n) = let g = gcd a (gcd b c)
                       sgn = signum c
                   in (sgn * (a `div` g), sgn * (b `div` g), sgn * (c `div` g), n)

-- find the continued fraction representation of (a + b*sqrt(n))/c
-- keep track of the remainders (a,b,c,n) in an accumulator so that
-- we can recognize when they start to repeat.
cont_frac_rep :: (Integral a) => [(a,(a,a,a,a))] -> (a,a,a,a) -> [(a,(a,a,a,a))]
cont_frac_rep acc (a,b,c,n)
    | (a,b,c,n) `elem` (map snd acc) = []
    | otherwise =
        let m = floor (toReal (a,b,c,n))
            next = reduce (((a-m*c)*c), (-b*c), ((a-m*c)^2 - b^2*n), n)
        in (m,next):(cont_frac_rep ((m,(a,b,c,n)):acc) next)

-- find the continued fraction representation of (a + b*sqrt(n))/c
-- keep track of the remainders (a,b,c,n) in an accumulator so that
-- we can recognize when they start to repeat.
cont_frac (a,b,c,n) =
          let m    = floor (toReal (a,b,c,n))
              next = reduce (((a-m*c)*c), (-b*c), ((a-m*c)^2 - b^2*n), n)
          in m:(cont_frac next)

nth' 1 (s:ss) = (1%1)/s
nth' n (s:ss) = (1%1)/(s + (nth' (n-1) ss))

nth 1 (s:ss) = s
nth n (s:ss) = s + nth' (n-1) ss

approximants :: Integral a
            => a
            -> [Ratio a]
approximants d = map (\nn -> nth nn $ map (%1) $ cont_frac (0,1,1,d)) [1..]

-- | solve pell's equation x^2-dy^2=1 where d is square free
pell :: Integral a
    => a        -- ^ a squarefree integer d
    -> Ratio a  -- ^ a fraction x % y where x^2-dy^2 = 1
pell d = head $ filter (\xx -> (numerator xx)^2 - d*(denominator xx)^2 == 1) $ approximants d

-- | return the number of primes less than or equal to n
-- by producing a list of such primes and calculating its
-- length.
num_primes_enumerate :: Integral a
                    => a
                    -> a
num_primes_enumerate n = genericLength $ takeWhile (<=n) primes

phi :: Integral a
    => [a]
    -> a
    -> a 
phi ps n = sum' [(fromIntegral sgn)*(n`div`pp) | (sgn,pp) <- bps2_w_sign n ps]


-- | return the number of primes less than or equal to n
-- using the inclusion exclusion method of Legendre.
--
-- This method works reasonably up to n around 10,000,000
-- after which we have memory issues.
num_primes_legendre :: Integral a
                    => a
                    -> a
num_primes_legendre n = nps + (phi ps n) - 1
                      where ps = takeWhile (<=sqrtInt n) primes
                            nps = genericLength ps


-- | return the number of primes less than or equal to n
-- using the inclusion exclusion method of Legendre.  For
-- use when you already have an increasing list of primes
-- up to (at least) sqrt(n).
--
num_primes_legendre_have_primes :: Integral a
                                => [a]
                                -> a
                                -> a
num_primes_legendre_have_primes ps' n = nps + (phi ps n) - 1
                    where ps = takeWhile (<=sqrtInt n) ps'
                          nps = genericLength ps

num_primes_meissel :: Integral a
                  => a
                  -> a
num_primes_meissel n = a + (phi psa n) - 1 - p2
                  where sqrtn = sqrtInt n
                        cbrtn = cbrtInt n
                        ps = takeWhile (<=sqrtn) primes
                        (psa,ps2) = span (<=cbrtn) ps
                        a = genericLength psa
                        b = genericLength ps
                        p2 = ((sum' [num_primes_legendre_have_primes ps (n`div`pb) | pb <- ps2]) + ((a*(a-1))`div`2) - ((b*(b-1))`div`2))

-- | form the list starting with two given elements where
-- | each element is the result of applying a given binary
-- | operator to the previous two elements of the list
fibonacci :: (a -> a -> a) -- ^ a binary operator
          -> a            -- ^ the first element of the list
          -> a            -- ^ the second element of the list
          -> [a]          -- ^ the resulting fibonacci sequence
fibonacci op a b = a:(fibonacci op b (a`op`b))

safe_head :: [a] -> Maybe a
safe_head []    = Nothing
safe_head (x:xs) = Just x

-- | return a primitive root of a
primitive_root :: Integral a
              => a
              -> Maybe a
primitive_root n = safe_head $ filter isprimitive $ filter (\k -> (gcd k n) == 1) [1..n]
              where phin = totient_function n
                    phin_prime_factors = fst $ factorize phin
                    isprimitive k = and [ (mod_pow n (phin`div`q) k) /= 1 | q <- phin_prime_factors ]
              

prop_squarefree_ints_test = all issquarefree (map product $ squarefree_ints_factorized 10000)

prop_padic_valuation_test1 :: (Integral a)
                          => a
                          -> Bool 
prop_padic_valuation_test1 n = padic_valuation 0 (n % 1) == ((abs n) % 1)

prop_num_primes_legendre_test = and $ map (\x -> (num_primes_legendre (10^x)) == num_primes_enumerate (10^x)) [1..5]

prop_num_primes_meissel_test  = and $ map (\x -> (num_primes_meissel  (10^x)) == num_primes_legendre  (10^x)) [4..7]

prop_sqrtInt_test = and $ map (\x -> (sqrtInt (x^2)) == x) [1..100000]

prop_cbrtInt_test = and $ map (\x -> (cbrtInt (x^3)) == x) [1..100000]

-- | the multiplicative group of \( Z/pZ \) is cyclic
prop_test = and $ map (\n -> (fmap (order n) (primitive_root n)) == Just (totient_function n)) (takeWhile (<=1000) primes)
