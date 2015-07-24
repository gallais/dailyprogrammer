{-# OPTIONS -Wall #-}

module Main where

import Data.Ratio
import Data.Natural

type PowerSeries = Natural -> Rational

atanPS :: PowerSeries
atanPS = maybe 0 (\ n -> ((- 1) ^ fromNatural n) % (2 * fromNatural n + 1)) . oddM

sumPS :: PowerSeries -> Rational -> Natural -> Rational
sumPS _  _ O     = 0
sumPS ps x (S n) = x ^ (fromNatural n) * ps n + sumPS ps x n

arctan :: Rational -> Natural -> Rational
arctan = sumPS atanPS

myPi :: Natural -> Rational
myPi = (4 *) . arctan 1

machin :: Natural -> Rational
machin n = (4 *) $ 4 * arctan (1 % 5) n - arctan (1 % 239) n

displayRational :: Natural -> Rational -> String
displayRational n r = show i ++ "." ++ decimals d q n
  where
    (i, d) = p `quotRem` q
    p      = numerator r
    q      = denominator r

    decimals :: Integer -> Integer -> Natural -> String
    decimals _ _ O     = ""
    decimals d q (S n) = show i' ++ decimals d' q n
      where (i' , d') = (10 * d) `quotRem` q

main :: IO ()
main = do
  let threshold   = toNatural 30
  let numberTerms = toNatural 50
  putStrLn $ displayRational threshold $ machin numberTerms
  putStrLn $ displayRational threshold $ myPi numberTerms
