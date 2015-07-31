-- https://www.reddit.com/r/dailyprogrammer/comments/qp3ub/392012_challenge_21_easy/

module Main where

import Data.List
import Data.Set

digits :: Integer -> [Int]
digits = fmap (read . return) . show

setOfDigits :: Integer -> Set Int
setOfDigits = fromList . digits

next :: Integer -> Integer
next n = head $ dropWhile ((dig /=) . setOfDigits) $ [(n+1)..]
  where dig = setOfDigits n

main :: IO ()
main = getLine >>= print . next . read
