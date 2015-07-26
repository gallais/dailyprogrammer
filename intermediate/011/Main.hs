-- https://www.reddit.com/r/dailyprogrammer/comments/pwox3/2192012_challenge_11_intermediate/

module Main where

rotate :: [Int] -> [Maybe Int]
rotate = reverse . fmap (flip lookup helper) where
  helper :: [(Int,Int)]
  helper = [ (0, 0), (1, 1), (2, 2), (5, 5)
           , (6, 9), (8, 8), (9, 6)]

isUpsideUp :: [Int] -> Bool
isUpsideUp ns = fmap Just ns == rotate ns

upsideUps :: Int -> Int -> [Int]
upsideUps m n =
    fmap (read . concat . fmap show)
      $ filter isUpsideUp
      $ fmap (fmap (read . (:[])) . show)
      $ [m..n]

main :: IO ()
main = do
  let numbers = upsideUps 0 10000
  print numbers
  print $ length numbers
