{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Monad.Logic
import           Data.Set            as Set
import           Data.Text           as T (Text, pack)
import           Data.Text.IO        as T
import           Pieces

choices
    :: MonadPlus m
    => [a] -> m a
choices = msum . fmap return

type Solution = ((Piece, Piece, Piece), (Piece, Piece, Piece), (Piece, Piece, Piece))

rotations
    :: MonadPlus m
    => Piece -> m Piece
rotations piece =
    choices
        [ piece
        , Piece
          { index = index piece
          , top = left piece
          , right = top piece
          , bottom = right piece
          , left = bottom piece
          }
        , Piece
          { index = index piece
          , top = bottom piece
          , right = left piece
          , bottom = top piece
          , left = right piece
          }
        , Piece
          { index = index piece
          , top = right piece
          , right = top piece
          , bottom = left piece
          , left = bottom piece
          }]

solution :: Logic Solution
solution = do
    otl <- choices pieces
    tl <- rotations otl
    otm <- choices pieces
    tm <- rotations otm
    guard $ connect (right tl) (left tm)
    otr <- choices pieces
    tr <- rotations otr
    guard $ connect (right tm) (left tr)
    oml <- choices pieces
    ml <- rotations oml
    guard $ connect (bottom tl) (top ml)
    omm <- choices pieces
    mm <- rotations omm
    guard $ connect (bottom tm) (top mm)
    guard $ connect (right ml) (left mm)
    omr <- choices pieces
    mr <- rotations omr
    guard $ connect (bottom tr) (top mr)
    guard $ connect (right mm) (left mr)
    obl <- choices pieces
    bl <- rotations obl
    guard $ connect (bottom ml) (top bl)
    obm <- choices pieces
    bm <- rotations obm
    guard $ connect (bottom mm) (top bm)
    guard $ connect (right bl) (left bm)
    obr <- choices pieces
    br <- rotations obr
    guard $ connect (bottom mr) (top br)
    guard $ connect (right bm) (left br)
    guard
        (Set.size (Set.fromList [otl, otm, otr, oml, omm, omr, obl, obm, obr]) ==
         9)
    return ((tl, tm, tr), (ml, mm, mr), (bl, bm, br))

showSolution :: Solution -> [Text]
showSolution (t, m, b) =
    mconcat [["----"], showTriple t, showTriple m, showTriple b]

showTriple :: (Piece, Piece, Piece) -> [Text]
showTriple (tl, tm, tr) =
    [ mconcat
          [ " "
          , suit (top tl)
          , "  "
          , " "
          , suit (top tm)
          , "  "
          , " "
          , suit (top tr)
          , " "]
    , mconcat
          [ suit (left tl)
          , idx (index tl)
          , suit (right tl)
          , " "
          , suit (left tm)
          , idx (index tm)
          , suit (right tm)
          , " "
          , suit (left tr)
          , idx (index tr)
          , suit (right tr)
          , " "]
    , mconcat
          [ " "
          , suit (bottom tl)
          , "  "
          , " "
          , suit (bottom tm)
          , "  "
          , " "
          , suit (bottom tr)
          , " "]
    , ""]
  where
    idx = T.pack . show
    suit (Heart, _) = "H"
    suit (Spade, _) = "S"
    suit (Club, _) = "C"
    suit (Diamond, _) = "D"

printSolution :: Solution -> IO ()
printSolution = mapM_ T.putStrLn . showSolution

solve :: IO ()
solve = printSolution (observe solution)

check :: IO ()
check = print (length (observeAll solution))
