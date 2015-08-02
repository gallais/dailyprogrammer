-- https://www.reddit.com/r/dailyprogrammer/comments/qp3ub/392012_challenge_21_easy/

module Main where

import Control.Monad
import Data.List   as List
import Data.Maybe
import Data.Set    as Set
import Data.Zipper as Zipper


digits :: Integer -> [Int]
digits = fmap (read . return) . show

unDigits :: [Int] -> Integer
unDigits = read . concatMap show

smallest :: [Int] -> Int -> [Int] -> Int -> Maybe Integer
smallest as b bs c = do
  guard (length digs <= length bs)
  return $ unDigits $ reverse as ++ c : rearranged
  where rearranged = reverse $ take (length bs) $ decreasing ++ repeat padding
        decreasing = sortBy (flip compare) digs
        digs       = (nub $ b : bs) List.\\ (nub $ c : as)
        padding    = minimum (b : as ++ bs)

next :: Integer -> Integer
next n = finish $ Zipper.foldr cons Nothing $ Zipper.fromList $ digs
  where
    cons as b bs = maybe (lookupGT b sdigs >>= smallest as b bs) Just
    digs   = digits n
    sdigs  = Set.fromList digs
    min    = findMin sdigs
    finish = fromMaybe (fromJust $ smallest [] min digs min)

main :: IO ()
main = getLine >>= print . next . read
