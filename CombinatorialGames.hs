module CombinatorialGames
       (
        mex
       ) where

import Data.Set (fromList,notMember)

mex' xs mx   = if mx `notMember` xs then mx else (mex' xs (mx+1))

mex xs = mex' xs 0
