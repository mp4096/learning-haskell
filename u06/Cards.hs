import           Data.List

data Suit = Club | Diamond | Spade | Heart deriving (Enum, Eq)
data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Enum, Eq, Ord, Bounded)
data Card = Card Suit Value deriving (Eq, Show)


instance Show Suit where
  show Club    = "♣"
  show Diamond = "♦"
  show Spade   = "♠"
  show Heart   = "♥"

instance Show Value where
  show Two   = "2"
  show Three = "3"
  show Four  = "4"
  show Five  = "5"
  show Six   = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine  = "9"
  show Ten   = "10"
  show Jack  = "J"
  show Queen = "Q"
  show King  = "K"
  show Ace   = "A"

instance Show Card where
  show (Card suit value) = show suit ++ " " ++ show value

instance Ord Card where
  compare (Card Heart left) (Card Heart right) = compare left right
  compare (Card _     _   ) (Card Heart _    ) = LT
  compare (Card Heart _   ) (Card _     _    ) = GT
  compare (Card _     left) (Card _     right) = compare left right

main :: IO ()
main = do
  let c1 = Card Club Two
      c2 = Card Diamond Ace
  print (c1 > c2)
  print
    (sort [ Card suit value | suit <- [Club .. Heart], value <- [Two .. Ace] ])
