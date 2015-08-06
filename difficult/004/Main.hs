-- https://www.reddit.com/r/dailyprogrammer/comments/pm7g7/2122012_challange_4_difficult/

module Main where

import Control.Monad
import Data.List

-- A small expression language with its evaluator

data BinOp = Add | Sub | Mul | Div
  deriving (Bounded, Enum, Eq)
data Arith = Lift Int | Op BinOp Arith Arith

instance Eq Arith where
  Lift k      == Lift l         = k == l
  (Op op a b) == (Op op' a' b') =
    op == op' && ((a == a' && b == b')
              ||  (op `elem` [Add, Mul] && a == b' && b == a'))

instance Show BinOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

instance Show Arith where
  show (Lift k)    = show k
  show (Op op a b) = show a ++ show op ++ show b

evalBinOp :: BinOp -> Int -> Int -> Maybe Int
evalBinOp Add a b = return $ a + b
evalBinOp Sub a b = return $ a - b
evalBinOp Mul a b = return $ a * b
evalBinOp Div a b = do
  guard (b /= 0)
  guard (a `mod` b == 0)
  return (a `quot` b)

evalArith :: Arith -> Maybe Int
evalArith (Lift k)      = Just k
evalArith (Op op ta tb) = do
  a <- evalArith ta
  b <- evalArith tb
  evalBinOp op a b


-- And now we try to find combinations of triples

select :: [a] -> [(a, [a])]
select []       = []
select (x : xs) = (x, xs) : fmap (fmap (x:)) (select xs)

three :: [Int] -> [(Arith,Arith)]
three xs = do
  (a, ys) <- select xs
  (b, zs) <- select ys
  (c, _)  <- select zs
  op      <- [minBound..maxBound]
  let left = Op op (Lift a) (Lift b)
  guard (evalArith left == Just c)
  return (left, Lift c)

main :: IO ()
main = do
  xs <- fmap read . words . filter (/= ',') <$> getLine
  mapM_ (uncurry $ \ a b -> putStrLn $ show a ++ " = " ++ show b) $ nub $ three xs
