-- https://www.reddit.com/r/dailyprogrammer/comments/pii6j/difficult_challenge_1/

module Main where

import System.IO

data Bounds =
  Bounds { lower :: Int
         , upper :: Int
         }

updateBounds :: Ordering -> Int -> Bounds -> Maybe Bounds
updateBounds ord k bnd =
  case ord of
    LT -> Just $ bnd { upper = min (k - 1) $ upper bnd }
    EQ -> Nothing
    GT -> Just $ bnd { lower = max (k + 1) $ lower bnd }

guessNumber :: Bounds -> (Bool, Int)
guessNumber bnd = (iAmSure, myGuess) where
  iAmSure = lower bnd == upper bnd
  myGuess = (upper bnd + lower bnd) `quot` 2

readAnswer :: IO Ordering
readAnswer = do
  c <- getChar
  putStrLn ""
  case c of
    'h' -> return GT
    'e' -> return EQ
    'l' -> return LT
    _   -> putStrLn "Malformed input!" >> readAnswer

loop :: Bounds -> IO ()
loop bnd = do
  let (b , n) = guessNumber bnd
  putStrLn $ "Is your number higher (h), equal to (e)\
             \ or lower than (l) " ++ show n ++ "?"
  ord <- readAnswer
  let bnd' = updateBounds ord n bnd
  maybe (putStrLn "Nice!")
        (if b then const (putStrLn "Bullshit!") else loop)
        bnd'

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  putStrLn "Pick a number between 1 and 100."
  putStrLn "I'm going to try to guess it!"
  loop $ Bounds 1 100
