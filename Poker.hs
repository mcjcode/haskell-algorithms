module Poker where

import Data.List

data Suit = S | H | D | C deriving (Show, Eq)

data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | RT | RJ | RQ | RK | RA deriving (Enum, Eq, Ord)

instance Show Rank where
    show R2 = "2"
    show R3 = "3"
    show R4 = "4"
    show R5 = "5"
    show R6 = "6"
    show R7 = "7"
    show R8 = "8"
    show R9 = "9"
    show RT = "T"
    show RJ = "J"
    show RQ = "Q"
    show RK = "K"
    show RA = "A"
    
data Card = Card Rank Suit

instance Show Card where
    show (Card r s) = (show r) ++ (show s)

charToSuit ch = case ch of
          'S' -> S
          'H' -> H
          'D' -> D
          'C' -> C
          _ -> error "error: bad character for Suit"

charToRank ch = case ch of
          '2' -> R2
          '3' -> R3
          '4' -> R4
          '5' -> R5
          '6' -> R6
          '7' -> R7
          '8' -> R8
          '9' -> R9
          'T' -> RT
          'J' -> RJ
          'Q' -> RQ
          'K' -> RK
          'A' -> RA
          _ -> error "error: bad character for Rank"

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | Straight | Flush | FullHouse | FourOfAKind | StraightFlush | RoyalFlush deriving (Show, Eq, Enum, Ord)

handType hand
         | isRoyalFlush hand = RoyalFlush
         | isStraightFlush hand = StraightFlush
         | isFourOfAKind hand = FourOfAKind
         | isFullHouse hand = FullHouse
         | isFlush hand = Flush
         | isStraight hand = StraightFlush
         | isThreeOfAKind hand = ThreeOfAKind
         | isTwoPair hand = TwoPair
         | isOnePair hand = OnePair
         | otherwise = HighCard

type HandRank = (HandType,[Rank])

handRank hand = (handType hand, rankFreqsZip hand)

strToCard str = Card (charToRank (str !! 0)) (charToSuit (str !! 1))

listFrom5tuple :: (a,a,a,a,a) -> [a]
listFrom5tuple (x1,x2,x3,x4,x5) = [x1,x2,x3,x4,x5]

type Hand = [Card]

cardToSuit (Card r s) = s
cardToRank (Card r s) = r

isFlush hand = length (nub $ map cardToSuit hand) == 1

isStraight hand = (lr <= RT) && ((sort (map cardToRank hand)) == [lr, succ lr, succ (succ lr), succ (succ (succ lr)), succ (succ (succ (succ lr)))]) where lr = lowRank hand

isStraightFlush hand = (isFlush hand) && (isStraight hand)

highRank :: Hand -> Rank
highRank = maximum . (map cardToRank)

lowRank :: Hand -> Rank
lowRank = minimum . (map cardToRank)

isRoyalFlush hand = (isFlush hand) && (isStraight hand) && (highRank hand == RA)

ranks :: Hand -> [Rank]
ranks = nub . (map cardToRank)

rankFreqs :: Hand -> ([Rank],[Int])
rankFreqs hand = let rks = ranks hand in (rks, map length [filter (==r) (map cardToRank hand) | r <- rks])

rankFreqsZip :: Hand -> [(Int,Rank)]
rankFreqsZip hand = reverse $ sort $ zip fqs rks where (rks,fqs) = rankFreqs hand

isFourOfAKind :: Hand -> Bool
isFourOfAKind  = (==[1,4]    ) . sort . snd . rankFreqs
isFullHouse    = (==[2,3]    ) . sort . snd . rankFreqs
isThreeOfAKind = (==[1,1,3]  ) . sort . snd . rankFreqs
isTwoPair      = (==[1,2,2]  ) . sort . snd . rankFreqs
isOnePair      = (==[1,1,1,2]) . sort . snd . rankFreqs


deck = [Card r s | r <- [R2 .. RA], s <- [S,H,D,C]]