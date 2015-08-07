-- https://www.reddit.com/r/dailyprogrammer/comments/pm7g7/2122012_challange_4_difficult/

module Main where

import Control.Monad
import Data.List
import Data.Tuple
import Language.Arithmetic

-- The ability to select a given number of values
-- from a list. All in the non-determinism monad
select :: [a] -> [(a, [a])]
select []       = []
select (x : xs) = (x, xs) : fmap (fmap (x:)) (select xs)

selects :: Int -> [a] -> [[a]]
selects 0 xs = return []
selects k xs = do
  (a , ys) <- select xs
  fmap (a :) $ selects (k - 1) ys

-- The ability to grow an arithmetic expression
-- at any leaf using arbitrary operations.
grow :: Int -> Arith -> [Arith]
grow l = go where
  andComm :: BinOp -> [(a,a)] -> [(a,a)]
  andComm op as =
    if op `elem` [Add, Mul] then as
    else as ++ fmap swap as

  go a@(Lift k) = do
    op     <- [minBound..maxBound]
    (e, f) <- andComm op [(a, Lift l)]
    return $ Op op e f
  go (Op op a b) = do
    fmap (\ a' -> Op op a' b) (go a)
    ++ fmap (\ b' -> Op op a b') (go b)

-- Growing multiple times
grows :: [Int] -> Arith -> [Arith]
grows ks a = foldr (concatMap . grow) [a] ks

-- Selecting interesting equations by growing
-- one k times and keeping it only if it is
-- equal to a given value
ariths :: Int -> [Int] -> [(Arith, Int)]
ariths k ks = do
  (e : f : gs) <- selects k ks
  a <- grows gs (Lift e)
  guard (evalArith a == Just f)
  return (a, f)

displayEquation :: Arith -> Int -> String
displayEquation a b = show a ++ " = " ++ show b

main :: IO ()
main = do
  xs <- fmap read . words . filter (/= ',') <$> getLine
  let threes  = nub $ ariths 3 xs
  let fours   = nub $ ariths 4 xs
  let allvals = nub $ ariths (length xs) xs
  mapM_ (uncurry $ \ name equations -> do
                     putStrLn ("*** " ++ name)
                     mapM_ (putStrLn . uncurry displayEquation) equations)
     [ ("threes", threes)
     , ("fours", fours)
     , ("allvals", allvals)
     ]
