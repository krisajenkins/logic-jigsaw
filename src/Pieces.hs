module Pieces where

data Suit
    = Club
    | Heart
    | Spade
    | Diamond
     deriving (Show, Eq, Ord)

data Piece = Piece
    { index  :: Int
    , top    :: (Suit,Bool)
    , right  :: (Suit,Bool)
    , bottom :: (Suit,Bool)
    , left   :: (Suit,Bool)
    } deriving (Show, Eq, Ord)

pieces :: [Piece]
pieces =
    [ Piece
      { index = 1
      , top = (Heart, True)
      , right = (Spade, True)
      , bottom = (Spade, False)
      , left = (Club, False)
      }
    , Piece
      { index = 2
      , top = (Heart, True)
      , right = (Diamond, True)
      , bottom = (Diamond, False)
      , left = (Heart, False)
      }
    , Piece
      { index = 3
      , top = (Diamond, True)
      , right = (Club, True)
      , bottom = (Club, False)
      , left = (Diamond, False)
      }
    , Piece
      { index = 4
      , top = (Club, True)
      , right = (Heart, True)
      , bottom = (Diamond, False)
      , left = (Club, False)
      }
    , Piece
      { index = 5
      , top = (Club, True)
      , right = (Heart, True)
      , bottom = (Spade, False)
      , left = (Heart, False)
      }
    , Piece
      { index = 6
      , top = (Spade, True)
      , right = (Diamond, True)
      , bottom = (Spade, False)
      , left = (Heart, False)
      }
    , Piece
      { index = 7
      , top = (Spade, True)
      , right = (Diamond, True)
      , bottom = (Heart, False)
      , left = (Diamond, False)
      }
    , Piece
      { index = 8
      , top = (Heart, True)
      , right = (Diamond, True)
      , bottom = (Club, False)
      , left = (Club, False)
      }
    , Piece
      { index = 9
      , top = (Spade, True)
      , right = (Spade, True)
      , bottom = (Heart, False)
      , left = (Club, False)
      }]

connect :: (Suit,Bool) -> (Suit,Bool) -> Bool
connect (suitA, directionA) (suitB, directionB) =
    (suitA == suitB) && (directionA /= directionB)
