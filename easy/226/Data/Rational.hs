module Data.Rational where

import Prelude hiding (Rational)
import Data.Maybe
import Data.List
import Data.Function

data Rational =
  Rational { numerator   :: Integer
           , denominator :: Integer }

instance Show Rational where
  show r = show (numerator r) ++ "/" ++ show (denominator r)

instance Read Rational where
  readsPrec _ s = [(Rational (read p) (read $ tail q), "")]
    where (p, q) = splitAt (fromJust $ elemIndex '/' s) s

add :: Rational -> Rational -> Rational
add p q = Rational ((r + s) `quot` gcdRS) (lcmPQ `quot` gcdRS)
  where lcmPQ = (lcm `on` denominator) p q
        r     = (lcmPQ `quot` denominator p) * numerator p
        s     = (lcmPQ `quot` denominator q) * numerator q
        rs    = r + s
        gcdRS = gcd (r + s) lcmPQ
