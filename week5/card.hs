import Data.List

data Suit = Clubs | Spades | Hearts | Diamonds
            deriving (Show, Eq, Enum, Bounded, Ord)
data Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Eq, Enum, Bounded, Ord)

data Card = Card Face Suit
  deriving (Show, Eq)

allCards :: [Card]
allCards = [Card face suit | suit <- [Spades,Clubs,Hearts,Diamonds], face <- [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]  ]


cmp1 :: Card -> Card -> Ordering
cmp1 (Card f1 s1) (Card f2 s2) =
    (case compare s1 s2 of
        EQ  -> compare f1 f2    -- if equal compare the faces
        ord -> ord) -- if different than use ord


cmp2 :: Card -> Card -> Ordering
cmp2 (Card f1 s1) (Card f2 s2) =
    (case compare f1 f2 of
        EQ  -> compare s1 s2    -- if equal compare the faces
        ord -> ord) -- if different than use ord