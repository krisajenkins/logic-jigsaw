{-# LANGUAGE OverloadedStrings #-}

module Pieces where

import           Data.String

import           Data.Text   (Text)
import qualified Data.Text   as T
import           Formatting  ((%))
import qualified Formatting  as F

data Suit
    = Club
    | Heart
    | Spade
    | Diamond
     deriving (Show, Eq, Ord)

type Edge = (Suit, Bool)

data Piece = Piece
    { index  :: Int
    , top    :: Edge
    , right  :: Edge
    , bottom :: Edge
    , left   :: Edge
    } deriving (Show, Eq, Ord)

type Solution = ((Piece, Piece, Piece), (Piece, Piece, Piece), (Piece, Piece, Piece))

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

canConnect :: Edge -> Edge -> Bool
canConnect (suitA, directionA) (suitB, directionB) =
    (suitA == suitB) && (directionA /= directionB)

rotate :: Piece -> Piece
rotate piece =
    Piece
    { index = index piece
    , top = left piece
    , right = top piece
    , bottom = right piece
    , left = bottom piece
    }

rotations :: Piece -> [Piece]
rotations piece =
    [ piece
    , rotate $ piece
    , rotate . rotate $ piece
    , rotate . rotate . rotate $ piece]

showSolution :: Solution -> [Text]
showSolution (t, m, b) =
    mconcat [["----"], showTriple t, showTriple m, showTriple b]

showTriple :: (Piece, Piece, Piece) -> [Text]
showTriple (tl, tm, tr) =
    [ F.sformat edgesFormat (top tl) (top tm) (top tr)
    , F.sformat
          centresFormat
          (left tl)
          (index tl)
          (right tl)
          (left tm)
          (index tm)
          (right tm)
          (left tr)
          (index tr)
          (right tr)
    , F.sformat edgesFormat (bottom tl) (bottom tm) (bottom tr)
    , ""]

edgeFormat :: F.Format r (Edge -> r)
edgeFormat = " " % formatEdge % " "

edgesFormat :: F.Format r (Edge -> Edge -> Edge -> r)
edgesFormat = edgeFormat % " " % edgeFormat % " " % edgeFormat

centreFormat :: F.Format r (Edge -> Int -> Edge -> r)
centreFormat = formatEdge % F.int % formatEdge

centresFormat
    :: F.Format r (Edge -> Int -> Edge -> Edge -> Int -> Edge -> Edge -> Int -> Edge -> r)
centresFormat = centreFormat % " " % centreFormat % " " % centreFormat

formatEdge :: F.Format r (Edge -> r)
formatEdge = F.later showEdge

showEdge
    :: IsString a
    => Edge -> a
showEdge (Heart, _) = "H"
showEdge (Spade, _) = "S"
showEdge (Club, _) = "C"
showEdge (Diamond, _) = "D"
