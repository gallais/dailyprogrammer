-- https://www.reddit.com/r/dailyprogrammer/comments/quli5/3132012_challenge_23_easy/

module Main where

import Data.List

split :: [a] -> ([a], Maybe a, [a])
split xs = if even n then (ys, Nothing, zs)
           else (ys, Just (head zs), tail zs)
  where
    n        = length xs
    (ys, zs) = splitAt (n `quot` 2) xs

display :: [String] -> [String] -> Maybe String -> [String] -> String
display xs ys z zs = concat
  [ "[", displayList xs, "]"
  , " = [", displayList ys, "]"
  , " ++ ", maybe "" (++ " : ") z
  , "[", displayList zs, "]" ]
  where displayList = concat . intersperse ", "

main :: IO ()
main = do
  xs <- fmap read getLine
  let (ys, z, zs) = split xs
  putStrLn $ display xs ys z zs
