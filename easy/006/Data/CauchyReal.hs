module Data.CauchyReal where

import Data.Natural
import Data.Ratio
import Data.Rational as R

type CauchyReal = [Rational]

approximate :: Natural -> CauchyReal -> Rational
approximate n (rat : rats) = go (truncateRational n rat) rats
  where
    go :: (Integer, Integer) -> [Rational] -> Rational
    go approx (r : rs) =
      let approx' = truncateRational n r in
      if approx == approx' then r else go approx' rs

display :: Natural -> CauchyReal -> String
display threshold = R.display threshold . approximate threshold
