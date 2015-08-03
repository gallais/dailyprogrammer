-- https://www.reddit.com/r/dailyprogrammer/comments/qheeu/342012_challenge_17_easy/

module Main where

import System.Environment
import Data.List
import Data.Text (Text , pack)
import Data.Text.IO as TIO (putStrLn)
import Data.Monoid

allLines :: [Text]
allLines = seed : unfoldr (Just . double) seed where
  seed       = pack "@"
  double str = let str' = str <> str in str' `seq` (str', str')

main :: IO ()
main = do
  (n : _) <- getArgs
  mapM_ TIO.putStrLn $ take (read n) allLines
