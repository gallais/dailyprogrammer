-- https://www.reddit.com/r/dailyprogrammer/comments/299hvt/6272014_challenge_168_easy_string_index/

module Main where

import Data.Char
import Data.List

normalise :: String -> String
normalise = fmap normChar where
  normChar c = if isAlpha c || isDigit c then c else ' '

nth :: [[a]] -> Int -> [a]
nth xs k = if test then xs !! (k - 1) else []
  where test = 0 < k && k <= length xs

main :: IO ()
main = do
  xs <- words . normalise . read <$> getLine
  ns <- fmap read . words <$> getLine
  putStrLn $ intercalate " " $ fmap (nth xs) ns
