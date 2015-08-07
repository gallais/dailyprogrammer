module Language.Arithmetic where

import Control.Monad

-- A small language for arithmetic expressions
data BinOp = Add | Sub | Mul | Div
  deriving (Bounded, Enum, Eq)
data Arith = Lift Int | Op BinOp Arith Arith

-- And its evaluator
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

-- Equality modulo commutativity
instance Eq Arith where
  Lift k      == Lift l         = k == l
  (Op op a b) == (Op op' a' b') =
    op == op' && ((a == a' && b == b')
              ||  (op `elem` [Add, Mul] && a == b' && b == a'))
  _           == _              = False

-- Pretty printing
instance Show BinOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

isLift :: Arith -> Bool
isLift (Lift _) = True
isLift _        = False

instance Show Arith where
  show (Lift k)    = show k
  show (Op op a b) = show' a ++ show op ++ show' b
    where
      show' a = if isLift a then show a
                else "(" ++ show a ++ ")"
