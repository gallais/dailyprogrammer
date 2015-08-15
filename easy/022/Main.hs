-- https://www.reddit.com/r/dailyprogrammer/comments/qr0hg/3102012_challenge_22_easy/

module Main where

setishUnion :: Eq a => [a] -> [a] -> [a]
setishUnion xs ys = xs ++ filter (not . (`elem` xs)) ys

main :: IO ()
main = do
  xs <- fmap words getLine
  ys <- fmap words getLine
  putStrLn $ unwords $ setishUnion xs ys
