-- https://www.reddit.com/r/dailyprogrammer/comments/pih8x/easy_challenge_1/

-- To add a bit of challenge, I decided to have a solution as generic as
-- possible: it should be possible to add new questions without having
-- to modify the main loop.

module Main where

import Data.Char
import Control.Monad

data Info = Name | Age | Username
  deriving (Show, Enum, Bounded)

pprint :: Info -> String
pprint = fmap toLower . show

display :: Info -> String -> String
display Name     name  = "your name is " ++ name
display Age      age   = ", you are " ++ age ++ " years old"
display Username uname = ", and your username is " ++ uname

interactions :: IO [String]
interactions =
  forM [minBound .. maxBound] $ \ i -> do
    putStrLn ("What is your " ++ pprint i ++ "?")
    getLine >>= return . display i

main :: IO ()
main = interactions >>= putStrLn . concat
