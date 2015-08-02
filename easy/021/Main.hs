-- https://www.reddit.com/r/dailyprogrammer/comments/qp3ub/392012_challenge_21_easy/

module Main where

import Data.List   as List
import Data.Maybe
import Data.Set    as Set
import Data.Zipper as Zipper


digits :: Integer -> [Int]
digits = fmap (read . return) . show

unDigits :: [Int] -> Integer
unDigits = read . concatMap show

smallest :: [Int] -> Int -> [Int] -> Int -> Integer
smallest as b bs c = unDigits $ reverse as ++ c : rearranged
  where rearranged = reverse $ take (length bs) decreasing
        decreasing = sortBy (flip compare) digs ++ repeat padding
        digs       = (nub $ b : bs) List.\\ (nub $ c : as)
        padding    = minimum (b : as ++ bs)

next :: Integer -> Integer
next n = finish $ Zipper.foldr cons Nothing $ Zipper.fromList $ digs
  where
    cons as b bs = maybe (fmap (smallest as b bs) $ lookupGT b sdigs) Just
    digs   = digits n
    sdigs  = Set.fromList digs
    min    = findMin sdigs
    finish = fromMaybe (unDigits $ min : digs)

main :: IO ()
main = getLine >>= print . next . read
