{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Control.Applicative
import           Control.Monad.Logic
import           Data.Monoid
import           Data.Set            as Set
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Pieces

choices
    :: MonadPlus m
    => [a] -> m a
choices = msum . fmap return

doesConnect
    :: Alternative f
    => (Suit, Bool) -> (Suit, Bool) -> f ()
doesConnect a b = guard $ canConnect a b

-- Lazy naming - sorry! In my defense, this is a quick lunchtime hack
-- and lunchtime is overrunning because of it!
--
-- tl is "top-left", mm is "middle-middle", etc.
-- otl is "original, unrotated top left", etc.
solution :: Logic Solution
solution = do
    otl <- choices pieces
    tl <- choices (rotations otl)
    otm <- choices pieces
    tm <- choices (rotations otm)
    doesConnect (right tl) (left tm)
    otr <- choices pieces
    tr <- choices (rotations otr)
    doesConnect (right tm) (left tr)
    oml <- choices pieces
    ml <- choices (rotations oml)
    doesConnect (bottom tl) (top ml)
    omm <- choices pieces
    mm <- choices (rotations omm)
    doesConnect (bottom tm) (top mm)
    doesConnect (right ml) (left mm)
    omr <- choices pieces
    mr <- choices (rotations omr)
    doesConnect (bottom tr) (top mr)
    doesConnect (right mm) (left mr)
    obl <- choices pieces
    bl <- choices (rotations obl)
    doesConnect (bottom ml) (top bl)
    obm <- choices pieces
    bm <- choices (rotations obm)
    doesConnect (bottom mm) (top bm)
    doesConnect (right bl) (left bm)
    obr <- choices pieces
    br <- choices (rotations obr)
    doesConnect (bottom mr) (top br)
    doesConnect (right bm) (left br)
    guard (countDistinct [otl, otm, otr, oml, omm, omr, obl, obm, obr] == 9)
    return ((tl, tm, tr), (ml, mm, mr), (bl, bm, br))
  where
    countDistinct
        :: Ord a
        => [a] -> Int
    countDistinct = Set.size . Set.fromList

printSolution :: Solution -> IO ()
printSolution = mapM_ T.putStrLn . showSolution

solve :: IO ()
solve = do
    mapM_ printSolution solutions
    T.putStrLn $
        "There were " <> T.pack (show (length solutions)) <>
        " solutions in total, including rotations."
  where
    solutions = observeAll solution

check :: IO ()
check = print (length (observeAll solution))
