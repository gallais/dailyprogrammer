module Data.Natural where

data Natural = O | S Natural

toNatural :: Integer -> Natural
toNatural 0         = O
toNatural n | n > 0 = S $ toNatural (n - 1)

fromNatural :: Natural -> Integer
fromNatural O     = 0
fromNatural (S n) = 1 + fromNatural n

evenM :: Natural -> Maybe Natural
evenM O     = Just O
evenM (S n) = fmap S $ oddM n

oddM :: Natural -> Maybe Natural
oddM O     = Nothing
oddM (S n) = evenM n
