module Main where

import System.Random
import System.Environment

randChar :: IO Char
randChar = fmap (chars !!) $ randomRIO (0, nchars - 1)
  where chars  = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "&#$£%@§!?*~ç^€<>{}();.,:/\\+=-[]"
        nchars = length chars

password :: Int -> IO String
password n = mapM (const randChar) [1..n]

main :: IO ()
main = do
  (m : n : _) <- getArgs
  passwords   <- mapM (const $ password $ read n) [1..(read m)]
  mapM_ putStrLn passwords

