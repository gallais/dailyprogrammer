module Main where

import System.Environment

main :: IO ()
main = do
  (fst : snd : _) <- getArgs
  putStrLn $ filter (not . (`elem` snd)) fst
