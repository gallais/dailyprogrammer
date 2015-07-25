module Data.Rational where

import Data.Natural
import Data.Ratio

truncateRational :: Natural -> Rational -> (Integer, Integer)
truncateRational n rat = (int, decimals dec den n 0)
  where
    (int, dec) = num `quotRem` den
    num        = numerator rat
    den        = denominator rat

    decimals :: Integer -> Integer -> Natural -> Integer -> Integer
    decimals _ _ O     acc = acc
    decimals d q (S m) acc = decimals rest q m (10 * acc + digit)
      where (digit, rest) = (10 * d) `quotRem` q

display :: Natural -> Rational -> String
display n r = show int ++ "." ++ show dec where
  (int, dec) = truncateRational n r
