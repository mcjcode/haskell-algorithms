module Text
       (
       csv_wrds,
       parse_mat
       ) where

-- | break a line of comma separated strings into a list of strings
csv_wrds   :: String   -- ^ the line of text to be broken into words 
           -> [String] -- ^ the list of strings
csv_wrds s =  case dropWhile (==',') s of
                   "" -> []
                   s' -> w : csv_wrds s''
                      where (w, s'') = break (==',') s'

-- | convert a block of text into a 'matrix' of integers
parse_mat :: [Char]      -- ^ the text to be parsed
          -> [[Integer]] -- ^ the matrix of integers
parse_mat text = [map read $ csv_wrds row_text | row_text <- lines text]