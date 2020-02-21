module Blackjack where

import Data.List


-- Data of a suit of a playing card --
data Suit = Heart | Diamond | Club | Spade
  deriving (Eq, Ord, Enum)

instance Show Suit where
  show Heart   = "\x2661"
  show Diamond = "\x2662"
  show Club    = "\x2663"
  show Spade   = "\x2660"

-- Data of royalty of a face card --
data Royalty = Jack | Queen | King
  deriving (Eq, Ord, Enum)

instance Show Royalty where
  show Jack  = "J"
  show Queen = "Q"
  show King  = "K"

-- Data of a playing card --
data Card = Number Int Suit
          | Face Royalty Suit
          | Ace Suit
  deriving (Eq, Ord)

instance Show Card where
  show (Number n s) = show n ++ show s
  show (Face r s)   = show r ++ show s
  show (Ace s)      = "A" ++ show s


-- A Deck and a Hand are both just lists of cards --
type Deck = [Card]
type Hand = [Card]


-- All the suits --
suits :: [Suit]
suits = [Heart .. ]

-- All the royalties --
faces :: [Royalty]
faces = [Jack .. ]

-- All the numbers --
numbers :: [Int]
numbers = [2 .. 10]

-- The full deck of cards! --
fullDeck :: Deck
fullDeck = [ Ace s | s <- suits ]
           ++
           [ Face r s | r <- faces, s <- suits ]
           ++
           [ Number n s | n <- numbers, s <- suits ]

-- The data of a (card/hand) score --
data Score = Score Int Int
  deriving (Show, Eq)

-- The first coordinate is hard score --
hard :: Score -> Int
hard (Score n _) = n

-- The second is soft score --
soft :: Score -> Int
soft (Score _ m) = m

-- The sum of hard and soft is the total score --
scoreValue :: Score -> Int
scoreValue x = hard x + soft x

-- If we are bust, get rid of some of the soft score (10 at a time) until we
-- are no longer bust, or until we don't have 10 soft score to get rid of
improveScore :: Score -> Score
improveScore x
  | scoreValue x <= 21 = x
  | soft x < 10        = x
  | otherwise          = improveScore (Score (hard x) (soft x - 10))

-- I can add scores together --
instance Monoid Score where
  mempty = Score 0 0
  mappend x y = Score (hard x + hard y) (soft x + soft y)

-- Get the score of a card --
cardScore :: Card -> Score
cardScore (Number n _) = Score n 0
cardScore (Face _ _)   = Score 10 0
cardScore (Ace _)      = Score 1 10

-- I can now calculate the score of a hand...
handScore :: Hand -> Score
handScore hand = mconcat [ cardScore card | card <- hand ]

-- ... and the value of a hand, i.e., best possible score taking both soft
-- score and hard score into account
handValue :: Hand -> Int
handValue = scoreValue . improveScore . handScore

-- Data of an indexed value --
-- I'm going to use index to sort/randomize.
data Indexed i a = Index i a
  deriving Show

getIndex :: Indexed i a -> i
getIndex (Index i _) = i

getValue :: Indexed i a -> a
getValue (Index _ a) = a

instance Eq i => Eq (Indexed i a) where
  x == y = getIndex x == getIndex y

instance Ord i => Ord (Indexed i a) where
  compare x y = compare (getIndex x) (getIndex y)

-- Shuffle a deck! --
-- match a list of is pairwise with a list of as
index :: [i] -> [a] -> [Indexed i a]
index _      []     = []
index []     _      = []
index (n:ns) (x:xs) = Index n x : index ns xs

-- forget the indices, only keep the values
getValues :: [Indexed i a] -> [a]
getValues xs = [ getValue x | x <- xs ]

shuffle :: Ord i => [i] -> [a] -> [a]
shuffle = curry (getValues . sort . uncurry index)
